# =====================================================
# DBSCAN - MODULO
# =====================================================


# -------------------------------
# TEORIA
# -------------------------------
DBSCAN_Teoria_UI <- function(id) {
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
        "Clustering Basado en Densidad (DBSCAN)",
        style = "font-weight: 800; color: #1a365d; margin-bottom: 5px;"
      ),
      
      p(
        "Algoritmo orientado a identificar regiones de alta densidad separadas por zonas de baja densidad, permitiendo descubrir formas arbitrarias y manejar el ruido.",
        style = "color: #64748b; font-size: 1.1rem; margin-bottom: 30px;"
      ),
      
      # =====================================
      # TARJETAS PRINCIPALES
      # =====================================
      bslib::layout_column_wrap(
        width = 1/3, # Tres columnas perfectamente niveladas
        heights_equal = "row",
        
        # ---------------------------------
        # 1. PARÁMETROS CRÍTICOS
        # ---------------------------------
        bslib::card(
          bslib::card_header(
            tags$b("1. Parámetros Fundamentales"),
            style = "background: #e0e7ff;"
          ),
          bslib::card_body(
            p("A diferencia de K-means, DBSCAN no requiere fijar el número de grupos a priori, sino que se parametriza globalmente mediante dos valores (Schubert et al., 2017):"),
            tags$ul(
              style = "margin-top: 10px;",
              tags$li(tags$b("Epsilon (\\(\\varepsilon\\)):"), " Define el radio de la vecindad. Determina la distancia máxima para considerar a dos puntos vecinos.", style = "margin-bottom: 8px;"),
              tags$li(tags$b("MinPts:"), " Número mínimo de puntos requeridos dentro del radio \\(\\varepsilon\\) (incluyéndose a sí mismo) para consolidar una región como densa e iniciar un clúster.")
            ),
            p("Matemáticamente, la vecindad de un punto \\(p\\) en un conjunto de datos \\(\\mathbf{D}\\) se define como:"),
            p("$$N_\\varepsilon(p) = \\{q \\in \\mathbf{D} \\mid d(p,q) \\le \\varepsilon\\}$$")
          )
        ),
        
        # ---------------------------------
        # 2. TIPOS DE PUNTOS
        # ---------------------------------
        bslib::card(
          bslib::card_header(
            tags$b("2. Tipologías de Individuos"),
            style = "background: #dcfce7;"
          ),
          bslib::card_body(
            p("En función del vecindario local dictado por \\(\\varepsilon\\) y \\(\\text{MinPts}\\), el algoritmo clasifica jerárquicamente cada observación en tres categorías excluyentes (Schubert et al., 2017):"),
            tags$ul(
              style = "margin-top: 10px;",
              tags$li(tags$b("Puntos Núcleo:"), " Individuos con una vecindad densa que cumple la condición estructural básica: \\(|N_\\varepsilon(p)| \\ge \\text{MinPts}\\).", style = "margin-bottom: 6px;"),
              tags$li(tags$b("Puntos Frontera:"), " Elementos que carecen de densidad suficiente (\\(|N_\\varepsilon(p)| < \\text{MinPts}\\)), pero pertenecen al radio de un punto núcleo \\(q\\) (\\(p \\in N_\\varepsilon(q)\\)).", style = "margin-bottom: 6px;"),
              tags$li(tags$b("Ruido:"), " Puntos que ni son núcleos ni se encuentran en la vecindad de uno; no pertenecen a ningún clúster.")
            )
          )
        ),
        
        # ---------------------------------
        # 3. SELECCIÓN DE EPSILON
        # ---------------------------------
        bslib::card(
          bslib::card_header(
            tags$b("3. Gráfico de k-distancias"),
            style = "background: #fef3c7;"
          ),
          bslib::card_body(
            p("Técnica empírica habitual para calibrar el valor del radio \\(\\varepsilon\\) óptimo (con \\(k = \\text{MinPts}\\)):"),
            tags$ul(
              style = "margin-top: 10px;",
              tags$li("Se calcula para cada punto la distancia a su \\(k\\)-ésimo vecino más cercano, representando posteriormente dichas distancias ordenadas de menor a mayor.", style = "margin-bottom: 8px;"),
              tags$li("El valor adecuado de \\(\\varepsilon\\) se identifica visualmente en la zona donde la curva presenta un cambio brusco de pendiente, conocido como codo.", style = "margin-bottom: 8px;"),
              tags$li("Los puntos previos a esta transición pertenecen generalmente a regiones densas, mientras que los valores elevados posteriores se asocian a ruido disperso.")
            )
          )
        )
        ),
        
      
      br(),
      
      # =================================================
      # ESQUEMA VISUAL DE CONCEPTOS DENSIDAD 
      # =================================================
     
      bslib::card(
        style = "border: 1px solid #cbd5e1; background: #ffffff;",
        bslib::card_body(
          h4(icon("eye"), "Conceptos Clave del Algoritmo DBSCAN", style = "color: #1a365d; font-weight: 700; text-align: center; margin-bottom: 20px;"),
          
          bslib::layout_column_wrap(
            width = 1/2,
            style = "gap: 20px;",
            
            # Bloque Izquierdo: Alcanzabilidad
            tags$div(
              style = "border: 1px solid #e2e8f0; padding: 15px; border-radius: 8px; background: #f8fafc; text-align: center;",
              tags$h5("Alcanzabilidad por Densidad", style = "color: #15803d; font-weight: bold; margin-bottom: 5px;"),
              tags$p("(Relación unidireccional y transitiva mediante saltos de densidad)", style = "font-size: 0.85rem; color: #64748b; margin-bottom: 15px;"),
              
              # Cadena horizontal limpia
              tags$div(
                style = "display: flex; justify-content: center; align-items: center; gap: 10px; margin: 35px 0;",
                tags$span(style = "width: 15px; height: 15px; background: #94a3b8; border-radius: 50%;"),
                tags$span(icon("arrow-right"), style = "color: #94a3b8; font-size: 0.8rem;"),
                tags$span("\\(p\\)", style = "width: 32px; height: 32px; background: #15803d; color: white; border-radius: 50%; display: flex; align-items: center; justify-content: center; font-weight: bold; font-size: 0.9rem;"),
                tags$span(icon("arrow-right"), style = "color: #15803d; font-weight: bold;"),
                tags$span("\\(r\\)", style = "width: 32px; height: 32px; background: #15803d; color: white; border-radius: 50%; display: flex; align-items: center; justify-content: center; font-weight: bold; font-size: 0.9rem;"),
                tags$span(icon("arrow-right"), style = "color: #15803d; font-weight: bold;"),
                tags$span("\\(s\\)", style = "width: 32px; height: 32px; background: #15803d; color: white; border-radius: 50%; display: flex; align-items: center; justify-content: center; font-weight: bold; font-size: 0.9rem;"),
                tags$span(icon("arrow-right"), style = "color: #15803d; font-weight: bold;"),
                tags$span("\\(q\\)", style = "width: 32px; height: 32px; background: #ea580c; color: white; border-radius: 50%; display: flex; align-items: center; justify-content: center; font-weight: bold; font-size: 0.9rem;")
              ),
              
              tags$div(
                style = "background: white; border: 1px solid #bbf7d0; padding: 10px; border-radius: 6px; font-size: 0.9rem;",
                tags$b("\\(q\\) es alcanzable desde \\(p\\)"), br(),
                "a través de una cadena de saltos de densidad (\\(p \\rightarrow r \\rightarrow s \\rightarrow q\\))."
              )
            ),
            
            # Bloque Derecho: Conectividad
            tags$div(
              style = "border: 1px solid #e2e8f0; padding: 15px; border-radius: 8px; background: #f8fafc; text-align: center;",
              tags$h5("Conectividad por Densidad", style = "color: #15803d; font-weight: bold; margin-bottom: 5px;"),
              tags$p("(Relación simétrica: existe un punto central que alcanza a ambos)", style = "font-size: 0.85rem; color: #64748b; margin-bottom: 15px;"),
              
              # Estructura horizontal corregida: p <- o -> q
              tags$div(
                style = "display: flex; justify-content: center; align-items: center; gap: 12px; margin: 35px 0;",
                tags$span("\\(p\\)", style = "width: 32px; height: 32px; background: #ea580c; color: white; border-radius: 50%; display: flex; align-items: center; justify-content: center; font-weight: bold; font-size: 0.9rem;"),
                tags$span(icon("arrow-left"), style = "color: #14532d; font-weight: bold;"),
                tags$span("\\(o\\)", style = "width: 36px; height: 36px; background: #14532d; color: white; border-radius: 50%; display: flex; align-items: center; justify-content: center; font-weight: bold; font-size: 1rem; border: 2px solid #bbf7d0; box-shadow: 0 0 8px rgba(22,101,52,0.2);"),
                tags$span(icon("arrow-right"), style = "color: #14532d; font-weight: bold;"),
                tags$span("\\(q\\)", style = "width: 32px; height: 32px; background: #ea580c; color: white; border-radius: 50%; display: flex; align-items: center; justify-content: center; font-weight: bold; font-size: 0.9rem;")
              ),
              
              tags$div(
                style = "background: white; border: 1px solid #ffedd5; padding: 10px; border-radius: 6px; font-size: 0.9rem;",
                tags$b("\\(p\\) y \\(q\\) están conectados por \\(o\\)"), br(),
                "porque ambos son alcanzables desde el punto núcleo \\(o\\) (\\(o \\rightarrow p\\) y \\(o \\rightarrow q\\))."
              )
            )
          ),
          
          # Nota informativa inferior
          tags$div(
            style = "margin-top: 15px; border-left: 4px solid #0284c7; background: #f0f9ff; padding: 10px; border-radius: 0 6px 6px 0; font-size: 0.88rem;",
            tags$b(icon("info"), "Nota teórica:"), 
            " Para que un punto inicial actúe como emisor en la cadena de alcanzabilidad, debe ser obligatoriamente un punto núcleo, asegurando que su vecindario \\(\\varepsilon\\) contenga al menos \\(\\text{MinPts}\\) observaciones."
          )
          
        )
      ),
      
      
      # =====================================
      # DINÁMICA DEL ALGORITMO Y FORMALIZACIÓN
      # =====================================
      bslib::card(
        style = "border: 1px solid #cbd5e1; background: #f8fafc;",
        bslib::card_body(
          h4(icon("gears"), "Algoritmo", style = "color: #1e40af; margin-bottom: 15px;"),
          p("El algoritmo DBSCAN construye las agrupaciones recorriendo secuencialmente la base de datos a partir de los parámetros de densidad establecidos (Pérez López & Santín González, 2007):"),
          
          tags$ul(
            style = "margin-top: 10px;",
            tags$li(tags$b("Evaluación de vecindad:"), " Selecciona un punto arbitrario \\(p\\) de la base de datos \\(\\mathbf{D}\\) y calcula su entorno local \\(N_\\varepsilon(p)\\). Si la cantidad de observaciones es menor a \\(\\text{MinPts}\\), se etiqueta provisionalmente como ruido y se evalúa otro punto.", style = "margin-bottom: 8px;"),
            tags$li(tags$b("Expansión recursiva:"), " Si la vecindad es densa (\\(|N_\\varepsilon(p)| \\ge \\text{MinPts}\\)), el punto se consolida como núcleo e inaugura un nuevo clúster. Automáticamente, el algoritmo incorpora de manera recursiva todos los puntos alcanzables por densidad desde \\(p\\), expandiendo las fronteras del grupo.", style = "margin-bottom: 8px;"),
            tags$li(tags$b("Reclasificación de fronteras:"), " Un punto calificado inicialmente como ruido puede cambiar de estado si es absorbido por el radio de expansión de un punto núcleo posterior, pasando a actuar de forma definitiva como punto frontera.", style = "margin-bottom: 8px;"),
            tags$li(tags$b("Fusión por umbrales globales:"), " Al usar valores fijos globales para \\(\\varepsilon\\) y \\(\\text{MinPts}\\) adaptados al clúster menos denso, dos clústeres contiguos se fusionarán si la distancia mínima entre sus elementos es menor o igual al radio especificado:"),
            p("$$d(\\mathbf{P}_1, \\mathbf{P}_2) = \\min \\{ d(p,q) \\mid p \\in \\mathbf{P}_1, \\, q \\in \\mathbf{P}_2 \\} \\le \\varepsilon$$")
          ),
          
          hr(style = "border-top: 1px solid #cbd5e1; margin: 20px 0;"),
          
          p("Bajo estas premisas de densidad, un clúster formal \\(\\mathbf{P} \\subseteq \\mathbf{D}\\) cumple rigurosamente las condiciones de (Ester et al., 1996):"),
          tags$div(
            style = "margin-left: 15px; margin-top: 10px; display: flex; flex-direction: column; gap: 10px; margin-bottom: 20px;",
            tags$div(
              style = "border-left: 4px solid #3b82f6; padding-left: 12px;",
              tags$b("Maximalidad: "), "Para todo \\(p, q \\in \\mathbf{D}\\), si \\(p \\in \\mathbf{P}\\) y \\(q\\) es alcanzable por densidad desde \\(p\\) respecto a \\(\\varepsilon\\) y \\(\\text{MinPts}\\), entonces invariablemente \\(q \\in \\mathbf{P}\\)."
            ),
            tags$div(
              style = "border-left: 4px solid #8b5cf6; padding-left: 12px;",
              tags$b("Conectividad: "), "Para todo par de observaciones \\(p, q \\in \\mathbf{P}\\), \\(p\\) debe estar conectado por densidad de forma simétrica con \\(q\\) con respecto a \\(\\varepsilon\\) y \\(\\text{MinPts}\\)."
            )
          ),
          
          p("Cualquier observación que al concluir las iteraciones globales no haya sido absorbida por las condiciones anteriores se aísla analíticamente bajo el conjunto de ruido matemático detectado:"),
          p("$$\\text{Ruido} = \\left\\{ p \\in \\mathbf{D} \\mid \\nexists \\, i : p \\in \\mathbf{P}_i \\right\\}$$")
        )
      ) 
    )
  ) 
} 

         
DBSCAN_Teoria_Server <- function(id){
  
  moduleServer(id, function(input, output, session){
    
  })
  
}



# -------------------------------
# ANALISIS
# -------------------------------

DBSCAN_Analisis_UI <- function(id){
  ns <- NS(id)
  tagList(
    # Título personalizado idéntico al de los módulos anteriores
    h3("Análisis", 
       style = "color: #1a446c; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; font-weight: 600; margin-top: 40px; margin-bottom: 20px; border-bottom: 2px solid #f4f6f9; padding-bottom: 10px;"),
    
    fluidRow(
      #--------------------------------------------------
      # PANEL LATERAL (A LA IZQUIERDA)
      #--------------------------------------------------
      column(4,
             wellPanel(
               h4("Configuración"),
               p("Establezca los parámetros de densidad para la detección de vecindades y puntos de ruido."),
               hr(),
               
               helpText("1. Ajusta minPts (Suele ser 2 * dimensiones)"),
               numericInput(ns("minPts"), "minPts (Min. puntos)", value = 5, min = 2),
               br(),
               
               helpText("2. Mira el gráfico k-dist y busca el 'codo' para el eps"),
               numericInput(ns("eps"), "eps (Radio de vecindad)", value = 0.5, min = 0.01, step = 0.05),
               
               # Se muestra condicionalmente en la pestaña de visualización
               conditionalPanel(
                 condition = sprintf("input['%s'] == '3. Clústeres Detectados'", ns("tabs_dbscan")),
                 uiOutput(ns("ui_var_cat"))
               ),
               
               hr(),
               downloadButton(ns("dl_db"), "Descargar Resultados (CSV)", class = "btn-success", style = "width: 100%;"),
               br(), br(),
               # Nota de consistencia idéntica a los demás módulos
               helpText("Nota: Se eliminan filas con valores faltantes automáticamente.")
             )
      ),
      
      #--------------------------------------------------
      # PANEL PRINCIPAL (A LA DERECHA)
      #--------------------------------------------------
      column(8,
             tabsetPanel(
               id = ns("tabs_dbscan"),
               
               tabPanel("1. Datos", 
                        br(),
                        p("Información: DBSCAN calcula las agrupaciones por densidad a partir de la matriz numérica normalizada.", 
                          style = "color: #7f8c8d; font-style: italic; margin-bottom: 25px;"),
                        h4("Dataset original preparado", style = "color: #2c3e50; font-weight: 500;"), 
                        DT::DTOutput(ns("tabla_datos")),
                        br(), hr(), br(),
                        h4("Dataset estandarizado (Z-scores)", style = "color: #2c3e50; font-weight: 500;"), 
                        DT::DTOutput(ns("tabla_scale"))),
               
               tabPanel("2. Optimización (k-dist)", 
                        br(),
                        plotOutput(ns("kdist_plot"), height = "500px"),
                        br(),
                        h4("Interpretación del Gráfico", style = "color: #2c3e50; font-weight: 500;"),
                        verbatimTextOutput(ns("interp_kdist"))),
               
               tabPanel("3. Clústeres Detectados", 
                        br(),
                        plotOutput(ns("cluster_plot"), height = "550px"),
                        br(), hr(), br(),
                        h4("Resumen de grupos", style = "color: #2c3e50; font-weight: 500;"), 
                        DT::DTOutput(ns("resumen_db")),
                        br(),
                        h4("Interpretación", style = "color: #2c3e50; font-weight: 500;"),
                        verbatimTextOutput(ns("interp_dbscan")))
             )
      )
    )
  )
}

DBSCAN_Analisis_Server <- function(id, datos, datos_ejemplo = NULL){
  moduleServer(id, function(input, output, session){
    
    # 1. DATOS BASE (Limpieza inicial para sincronizar filas)
    datos_base <- reactive({
      df <- if(!is.null(datos()) && nrow(datos()) > 0) datos() else datos_ejemplo
      req(df)
      df[complete.cases(df), , drop = FALSE]
    })
    
    # 2. DATOS NUMÉRICOS (Solo para el cálculo)
    datos_num <- reactive({
      df <- datos_base()
      df_n <- df[, sapply(df, is.numeric), drop = FALSE]
      cols_validas <- sapply(df_n, function(x) length(unique(x)) > 15)
      if(sum(cols_validas) < 2) return(df_n)
      df_n[, cols_validas, drop = FALSE]
    })
    
    datos_scale <- reactive({ 
      req(ncol(datos_num()) >= 2)
      scale(datos_num()) 
    })
    
    # 3. PCA (Cálculo independiente para visualización)
    pca_res <- reactive({ 
      req(datos_scale())
      prcomp(datos_scale()) 
    })
    
    # 4. MODELO DBSCAN
    modelo_db <- reactive({
      req(datos_scale(), input$eps, input$minPts)
      dbscan::dbscan(datos_scale(), eps = input$eps, minPts = input$minPts)
    })
    
    # 5. UNIÓN DE DATOS
    df_final <- reactive({
      req(pca_res(), modelo_db(), datos_base())
      
      df_viz <- as.data.frame(pca_res()$x[, 1:2])
      df_viz$cluster <- factor(modelo_db()$cluster)
      
      if(!is.null(input$var_cat) && input$var_cat != "" && input$var_cat != "Ninguna") {
        df_viz$realidad <- as.factor(datos_base()[[input$var_cat]])
      }
      df_viz
    })
    
    # --- 6. SELECTOR DINÁMICO ---
    output$ui_var_cat <- renderUI({
      df <- datos_base()
      nom_cats <- names(df)[sapply(df, function(x) {
        is.factor(x) || is.character(x) || (is.numeric(x) && length(unique(x)) <= 5)
      })]
      
      selectInput(session$ns("var_cat"), "Comparar con Variable Real:", 
                  choices = c("Ninguna", nom_cats), 
                  selected = "Ninguna") 
    })
    
    # --- 7. TABLAS DE DATOS (CORREGIDO: ORIGINAL COMPLETO VS NUMÉRICO PURO) ---
    output$tabla_datos <- DT::renderDT({
      DT::datatable(
        datos_base(), # Dataset original entero con sus categorías de control
        options = list(paging = FALSE, scrollY = "400px", scrollX = TRUE, autoWidth = TRUE),
        class = 'cell-border stripe hover compact'
      ) %>% 
        DT::formatRound(columns = names(datos_num()), digits = 3)
    })
    
    output$tabla_scale <- DT::renderDT({
      req(datos_scale())
      DT::datatable(
        round(as.data.frame(datos_scale()), 3), # Matriz pura de Z-scores numéricos
        options = list(paging = FALSE, scrollY = "400px", scrollX = TRUE, autoWidth = TRUE),
        class = 'cell-border stripe hover compact'
      )
    })
    
    # --- 8. TIMING OPTIMIZACIÓN (k-dist) ---
    output$kdist_plot <- renderPlot({
      req(datos_scale())
      dbscan::kNNdistplot(datos_scale(), k = input$minPts)
      abline(h = input$eps, col = "red", lty = 2, lwd = 2)
      title(main = paste("Gráfico de k-distancias (minPts =", input$minPts, ")"))
    })
    
    output$interp_kdist <- renderText({
      req(input$minPts)
      paste0(
        "El gráfico calcula las distancias de cada objeto a su k-ésimo vecino más cercano ordenadas de menor a mayor.\n",
        "El valor ideal del radio 'eps' se localiza visualmente en la zona de inflexión pronunciada de la curva (el codo).\n\n",
        "Ejemplo práctico (Dataset 'penguins'): Con minPts = 5, notarás que la curva asciende de forma suave y plana ",
        "antes de dispararse de forma casi vertical. El punto donde empieza esa subida abrupta (el codo) te indicará ",
        "el radio de vecindad 'eps' óptimo para capturar la densidad de los grupos principales."
      )
    })
    
    # --- 9. RENDERIZADO DEL GRÁFICO ---
    output$cluster_plot <- renderPlot({
      req(df_final())
      df <- df_final()
      
      p <- ggplot(df, aes(x = PC1, y = PC2))
      
      if(!is.null(input$var_cat) && input$var_cat != "" && input$var_cat != "Ninguna") {
        p <- p + geom_point(aes(color = realidad, shape = cluster), size = 3.5, alpha = 0.8) +
          labs(color = input$var_cat, shape = "Clúster (0=Ruido)")
      } else {
        p <- p + geom_point(aes(color = cluster), size = 3, alpha = 0.7) +
          labs(color = "Clúster (0=Ruido)")
      }
      
      p + theme_minimal() + 
        scale_color_brewer(palette = "Set1") +
        labs(title = "DBSCAN: Resultados Proyectados en PCA",
             subtitle = "Nota: El Clúster 0 representa el RUIDO (Outliers detectados)")
    })
    
    # --- 10. TABLA RESUMEN COMPACTA ---
    output$resumen_db <- DT::renderDT({
      req(modelo_db())
      tab <- as.data.frame(table(Clúster = modelo_db()$cluster))
      colnames(tab) <- c("Clúster (0 = Ruido)", "Nº de Individuos")
      
      DT::datatable(
        tab,
        options = list(paging = FALSE, scrollX = TRUE, autoWidth = TRUE),
        class = 'cell-border stripe hover compact'
      )
    })
    
    output$interp_dbscan <- renderText({
      req(input$eps, input$minPts)
      paste0(
        "A diferencia de K-means o Jerárquico, DBSCAN no requiere fijar los grupos de antemano; los descubre por densidad ",
        "y aísla los puntos periféricos que no cumplen las condiciones como Clúster 0 (Ruido).\n\n",
        "Ejemplo práctico (Dataset 'penguins'): Con un ajuste preciso de 'eps' y 'minPts', DBSCAN identificará de ",
        "forma nativa la alta densidad de las especies de pingüinos. Verás que el Clúster 0 (puntos rojos sueltos) ",
        "captura ejemplares con dimensiones híbridas o inusuales (outliers morfológicos), aislando el ruido sin forzar ",
        "su inclusión en los grupos principales y aumentando la pureza de los clústeres densos."
      )
    })
    
    # --- 11. BOTÓN DE DESCARGA ---
    output$dl_db <- downloadHandler(
      filename = function() { paste("Análisis_DBSCAN_", Sys.Date(), ".csv", sep="") },
      content = function(file) {
        df_out <- datos_base()
        df_out$DBSCAN_Cluster <- modelo_db()$cluster
        write.csv(df_out, file, row.names = FALSE)
      }
    )
    
  })
}


# -------------------------------
# AUTOEVALUACION
# -------------------------------

DBSCAN_Auto_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # ─── SOLUCIÓN REFORZADA DEFINTIVA PARA RESPUESTAS EN UNA LÍNEA ───
    tags$head(
      tags$style(HTML("
        /* Obliga a los contenedores de radio buttons a usar todo el ancho disponible */
        .shiny-input-radiogroup, 
        .shiny-input-container,
        .shiny-options-group {
          width: 100% !important;
          max-width: 100% !important;
        }
        
        /* Ajuste estructural para Bootstrap 5 / bslib */
        .shiny-input-radiogroup .form-check,
        .shiny-input-radiogroup .radio {
          display: flex !important;
          align-items: flex-start !important; /* Mantiene el círculo arriba si hay texto largo */
          width: 100% !important;
          max-width: 100% !important;
          gap: 0.5rem;
          margin-bottom: 8px;
        }
        
        /* Fuerza a la etiqueta de texto a expandirse sin restricciones ocultas */
        .shiny-input-radiogroup .form-check-label,
        .shiny-input-radiogroup label {
          flex: 1 1 auto !important;
          width: auto !important;
          white-space: normal !important; 
          word-break: break-word !important;
          display: inline-block !important;
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
        title = "➕ Gestión: Añadir pregunta personalizada de DBSCAN", # Corrigo texto PCA -> DBSCAN
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
        
        actionButton(ns("add"), "Guardar pregunta en el banco de DBSCAN", class = "btn-success btn-sm mt-2") # Corrigo texto PCA -> DBSCAN
      )
    )
  )
}

DBSCAN_Auto_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # CORRECCIÓN 1: Habías duplicado e inicializado dos veces consecutivas este valor reactivo
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
        texto = "¿Cuál es el objetivo principal del DBSCAN?",
        opciones = c("Agrupar observaciones basándose en regiones de alta densidad de puntos", "Calcular una matriz de cargas y factores comunes", "Ajustar una línea de regresión por mínimos cuadrados", "Reducir la dimensionalidad maximizando la varianza"),
        correcta = "Agrupar observaciones basándose en regiones de alta densidad de puntos"
      ),
      list(
        texto = "¿Qué son el radio (Épsilon) y MinPts en DBSCAN?",
        opciones = c("Los parámetros para medir los autovalores de la matriz identidad", "Los dos parámetros fundamentales para definir la vecindad y densidad mínima", "El número máximo de iteraciones y clusters predefinidos", "Los errores estadísticos de la covarianza residual"),
        correcta = "Los dos parámetros fundamentales para definir la vecindad y densidad mínima"
      ),
      list(
        texto = "¿Qué es el ruido (Noise) en DBSCAN?",
        opciones = c("Variables redundantes que deben ser eliminadas con rotación Varimax", "Puntos que no pertenecen a ningún cluster por no cumplir los requisitos de densidad", "Errores aleatorios de medición en las medias muestrales", "Observaciones repetidas o duplicadas en la matriz original"),
        correcta = "Puntos que no pertenecen a ningún cluster por no cumplir los requisitos de densidad"
      ),
      list(
        texto = "¿Dónde se encuentra un punto frontera (Border Point) en DBSCAN?",
        # CORRECCIÓN 2: En la lista tenías escrito "of" en inglés dentro de la respuesta correcta ("vecindad of un punto..."), 
        # pero en las opciones decía "de" ("vecindad de un punto..."). Al no coincidir el texto exacto, la corrección siempre daba "Incorrecto".
        opciones = c("En el centro geométrico exacto de la agrupación o cluster", "Dentro del radio de vecindad de un punto núcleo, pero sin tener suficientes vecinos propios", "Fuera de cualquier radio de densidad, considerándose un outlier absoluto", "En la intersección ortogonal entre dos componentes principales"),
        correcta = "Dentro del radio de vecindad de un punto núcleo, pero sin tener suficientes vecinos propios"
      ),
      list(
        texto = "¿Cuál es la principal diferencia entre el DBSCAN y el K-Means?",
        opciones = c("DBSCAN no requiere definir el número de clusters de antemano y detecta formas arbitrarias", "K-Means permite identificar ruido y DBSCAN obliga a agrupar todos los puntos", "DBSCAN solo funciona con clusters compactos de forma puramente esférica", "No hay diferencias, ambos algoritmos calculan centroides de medias de forma idéntica"),
        correcta = "DBSCAN no requiere definir el número de clusters de antemano y detecta formas arbitrarias"
      ),
      list(
        texto = "Hay un punto núcleo p en un cluster. ¿Dónde están los demás puntos del cluster con respecto a p?",
        opciones = c("En una disposición espacial oblicua o factorial paralela", "Son alcanzables directa o indirectamente por densidad a través de cadenas de puntos núcleos", "Agrupados exactamente a la misma distancia euclídea del origen", "Distribuidos siguiendo una curva normal simétrica perfecta"),
        correcta = "Son alcanzables directa o indirectamente por densidad a través de cadenas de puntos núcleos"
      ),
      list(
        texto = "¿Qué ocurre si se selecciona un valor de Épsilon (radio) demasiado pequeño?",
        opciones = c("La inercia de los clusters se reduce a cero de forma inmediata", "La gran mayoría de los puntos se clasificarán erróneamente como ruido", "Todos los puntos se fusionarán en un único cluster esférico", "El algoritmo requerirá un mayor número de centroides iniciales"),
        correcta = "La gran mayoría de los puntos se clasificarán erróneamente como ruido"
      ),
      list(
        texto = "¿Qué herramienta gráfica se utiliza comúnmente para estimar el valor óptimo de Épsilon?",
        opciones = c("El gráfico de sedimentación (Scree plot)", "El gráfico de distancias al k-ésimo vecino más cercano (K-distance plot)", "El dendrograma de enlaces y distancias cophenéticas", "La curva de varianza acumulada de las cargas factoriales"),
        correcta = "El gráfico de distancias al k-ésimo vecino más cercano (K-distance plot)"
      ),
      list(
        texto = "Si un punto no es núcleo pero cae dentro del radio de dos puntos núcleos de clusters diferentes, ¿cómo actúa DBSCAN de forma estándar?",
        opciones = c("El punto se elimina del dataset por ser considerado ruido no lineal", "Se asigna al primer cluster que lo explore en el orden del algoritmo o al más cercano", "Duplica de forma automática el valor del parámetro MinPts", "Divide de forma simétrica el cluster original en dos partes"),
        correcta = "Se asigna al primer cluster que lo explore en el orden del algoritmo o al más cercano"
      ),
      list(
        texto = "¿Por qué el escalado de variables es crítico en DBSCAN?",
        opciones = c("Porque utiliza métricas como la distancia euclídea, por lo que diferencias de escala distorsionan el radio Épsilon", "Porque permite calcular los eigenvalores mayores que uno", "Porque el algoritmo exige que los datos estén centrados en cero obligatoriamente", "No es crítico, DBSCAN es totalmente inmune al orden de magnitude de las variables"),
        correcta = "Porque utiliza métricas como la distancia euclídea, por lo que diferencias de escala distorsionan el radio Épsilon"
      ),
      list(
        texto = "¿A qué gran familia de algoritmos de clustering pertenece DBSCAN?",
        opciones = c("Algoritmos de particionamiento basados en centroides", "Algoritmos basados en densidad", "Métodos jerárquicos aglomerativos", "Modelos de mixtura gaussiana supervisada"),
        correcta = "Algoritmos basados en densidad"
      ),
      list(
        texto = "¿Qué se recomienda generalmente como regla básica para fijar el valor de MinPts?",
        opciones = c("Debe ser mayor o igual a la dimensión de los datos más 1 (Dim + 1)", "Debe ser igual al 50% de las observaciones totales", "Un valor fijo de cero para maximizar la detección de ruido", "Depende del número de componentes principales elegidas"),
        correcta = "Debe ser mayor o igual a la dimensión de los datos más 1 (Dim + 1)"
      ),
      list(
        texto = " ¿Qué tipo de estructuras o formas geométricas puede detectar DBSCAN con éxito?",
        opciones = c("Únicamente agrupaciones compactas y esféricas", "Formas arbitrarias, complejas, alargadas o anillos concéntricos", "Solamente líneas rectas de regresión ortogonal", "Estructuras basadas en árboles de decisión binarios"),
        correcta = "Formas arbitrarias, complejas, alargadas o anillos concéntricos"
      ),
      list(
        texto = "¿Cuál es una de las principales limitaciones de DBSCAN?",
        opciones = c("Requiere que el usuario defina la matriz de confusión inicial", "Su rendimiento decae severamente en conjuntos de datos con densidades muy variables", "No es capaz de clasificar ninguna observación como outlier", "Obliga a que todos los grupos calculados contengan el mismo número de puntos"),
        correcta = "Su rendimiento decae severamente en conjuntos de datos con densidades muy variables"
      ),
      list(
        texto = "Si aumentamos significativamente el valor de MinPts manteniendo Épsilon constante, ¿qué efecto se produce?",
        opciones = c("Se formarán más clusters y de menor tamaño", "El algoritmo se volverá más restrictivo y aumentará la cantidad de puntos classified como ruido", "Todos los puntos del mapa pasarán a ser automáticamente puntos núcleos", "Se anulará por completo la métrica de distancia euclídea"),
        correcta = "El algoritmo se volverá más restrictivo y aumentará la cantidad de puntos clasificados como ruido"
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
    
    # CORRECCIÓN 3: Cambiado observe() por observeEvent(preguntas(), ...).
    # Al usar observe() plano con isolate(), Shiny entraba en bucles infinitos de renderizado
    # o no actualizaba correctamente el set inicial de preguntas al iniciar el módulo.
    observeEvent(preguntas(), {
      lista_actual <- preguntas()
      
      lista_enriquecida <- lapply(lista_actual, function(p) {
        p$id_unico <- paste0("q_", gsub("[^a-zA-Z0-9]", "", p$texto))
        p
      })
      
      if (is.null(preguntas_ordenadas())) {
        preguntas_ordenadas(sample(lista_enriquecida, min(10, length(lista_enriquecida))))
      }
    }, ignoreNULL = FALSE)
    
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

    
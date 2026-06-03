# =====================================================
#  Árboles de decisión
# =====================================================

# -------------------------------
# TEORIA
# -------------------------------

Arboles_Teoria_UI <- function(id) {
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
        "Árboles de Clasificación",
        style = "font-weight: 800; color: #1a365d; margin-bottom: 5px;"
      ),
      
      p(
        "Modelos predictivos basados en el aprendizaje supervisado que descubren de forma recursiva reglas de asociación estructuradas en flujos lógicos.",
        style = "color: #64748b; font-size: 1.1rem; margin-bottom: 30px;"
      ),
      
      # =====================================
      # TARJETAS PRINCIPALES
      # =====================================
      bslib::layout_column_wrap(
        width = 1/3, # Tres columnas perfectamente alineadas
        heights_equal = "row",
        
        # ---------------------------------
        # 1. ¿QUÉ ES UN ÁRBOL?
        # ---------------------------------
        bslib::card(
          bslib::card_header(
            tags$b("1. Estructura y Flujo"),
            style = "background: #e0e7ff;"
          ),
          bslib::card_body(
            p("Técnica que segmenta jerárquicamente la muestra traduciendo las etiquetas conocidas de \\(Y\\) según sus atributos predictivos (Breiman et al., 2017). Se compone de:"),
            tags$ul(
              style = "margin-top: 10px;",
              tags$li(tags$b("Nodos y Ramas:"), " El nodo raíz (superior) e intermedios denotan pruebas lógicas sobre los atributos; las ramas bifurcan sus resultados.", style = "margin-bottom: 6px;"),
              tags$li(tags$b("Hojas:"), " Nodos terminales que contienen la clase o categoría final asignada al individuo.")
            )
          )
        ),
        
        # ---------------------------------
        # 2. INDUCCIÓN E HISTORIA
        # ---------------------------------
        bslib::card(
          bslib::card_header(
            tags$b("2. Algoritmos Fundamentales"),
            style = "background: #dcfce7;"
          ),
          bslib::card_body(
            p("Desarrollados de manera independiente entre finales de los 70 y principios de los 80, dieron pie al grueso de la inducción moderna (Han et al., 2011):"),
            tags$ul(
              style = "margin-top: 10px;",
              tags$li(tags$b("Línea ID3 / C4.5:"), " Diseñados por J. Ross Quinlan, basados en el particionamiento multi-rama a través de la ganancia de información.", style = "margin-bottom: 6px;"),
              tags$li(tags$b("Algoritmo CART:"), " Propuesto por Breiman, enfocado explícitamente en la generación iterativa de árboles de decisión binarios.")
            )
          )
        ),
        
        # ---------------------------------
        # 3. ATRIBUTOS CRÍTICOS
        # ---------------------------------
        bslib::card(
          bslib::card_header(
            tags$b("3. Ventajas y Rendimiento"),
            style = "background: #fef3c7;"
          ),
          bslib::card_body(
            p("Esta metodología goza de una amplia adopción en minería de datos debido a sus propiedades intrínsecas (Han et al., 2011):"),
            tags$ul(
              style = "margin-top: 10px;",
              tags$li(tags$b("Cero caja negra:"), " Proporciona gráficos e interpretaciones intuitivas, idóneas para hallar patrones ocultos.", style = "margin-bottom: 6px;"),
              tags$li(tags$b("Sin configuración:"), " No exige calibrar parámetros previos estrictos para entrenar sus reglas iniciales.", style = "margin-bottom: 6px;"),
              tags$li(tags$b("Robustez:"), " Destaca por su fiabilidad y precisión en entornos multivariantes.")
            )
          )
        )
      ), # Fin del layout_column_wrap
      
      br(),
      
      # =====================================
      # CRITERIOS DE PARTICIÓN
      # =====================================
      bslib::card(
        style = "border: 1px solid #cbd5e1; background: #f8fafc;",
        bslib::card_body(
          h4(icon("chart-pie"), "Criterios de Partición y Medidas de Impureza", style = "color: #1e40af; margin-bottom: 15px;"),
          p("Para realizar las divisiones lógicas, el algoritmo busca aislar las clases de forma que cada subnodo sea lo más homogéneo ('puro') posible, apoyándose en dos indicadores analíticos principales:"),
          
          bslib::layout_column_wrap(
            width = 1/2,
            style = "gap: 20px; margin-top: 15px;",
            
            # Subcolumna: Entropía
            tags$div(
              style = "border: 1px solid #e2e8f0; padding: 15px; border-radius: 8px; background: white;",
              tags$h5(tags$b("Entropía y Ganancia de Información"), style = "color: #1a365d; margin-bottom: 10px;"),
              p("La entropía mide el grado de incertidumbre o desorden del subconjunto de datos \\(\\mathbf{S}\\):"),
              p("$$H(\\mathbf{S}) = -\\sum_{i=1}^{c} p_i \\log_2 p_i$$"),
              p("Donde \\(c\\) es el total de clases e \\(p_i\\) es la proporción del grupo \\(i\\). A partir de ella, se evalúa un atributo \\(A\\) buscando el que ", tags$b("maximice la ganancia"), " de reducción de desorden:"),
              p("$$\\text{Gain}(\\mathbf{S}, A) = H(\\mathbf{S}) - \\sum_{v \\in \\text{Valores}(A)} \\frac{|\\mathbf{S}_v|}{|\\mathbf{S}|} H(\\mathbf{S}_v)$$")
            ),
            
            # Subcolumna: Gini
            tags$div(
              style = "border: 1px solid #e2e8f0; padding: 15px; border-radius: 8px; background: white;",
              tags$h5(tags$b("Índice de Gini"), style = "color: #1a365d; margin-bottom: 10px;"),
              p("Utilizado de forma predeterminada por el algoritmo CART, mide directamente la impureza o grado de mezcla de clases del nodo:"),
              p("$$\\text{Gini}(\\mathbf{S}) = 1 - \\sum_{i=1}^{c} p_i^2$$"),
              p("Este índice arroja valores mínimos (cercanos a 0) si la inmensa mayoría de las observaciones pertenecen a una única categoría. Aquí, el algoritmo selecciona el atributo que ", tags$b("minimice el índice de Gini"), ", forzando particiones lo más puras posibles.")
            )
          )
        )
      ),
      
      br(),
      
      # =====================================
      # DINÁMICA DE CONSTRUCCIÓN Y PARADA
      # =====================================
      bslib::card(
        style = "border: 1px solid #cbd5e1; background: #f8fafc;",
        bslib::card_body(
          h4(icon("gears"), "Construcción Recursiva y Condiciones de Parada", style = "color: #1e40af; margin-bottom: 15px;"),
          p("El proceso sigue un enfoque recursivo descendente que arranca desde el nodo raíz con la totalidad de la muestra y divide los datos de forma iterativa según la tipología de las variables:"),
          tags$ul(
            tags$li(tags$b("Variables categóricas:"), " Se abren simultáneamente tantas ramas físicas como valores distintos posea el atributo evaluado.", style = "margin-bottom: 6px;"),
            tags$li(tags$b("Variables continuas:"), " Se calcula un punto de corte óptimo que segmenta los datos en dos subgrupos, originando particiones binarias.")
          ),
          
          hr(style = "border-top: 1px solid #cbd5e1; margin: 15px 0;"),
          
          h5("Condiciones de Parada Estructurales:", style = "color: #1a365d; font-weight: bold; margin-bottom: 10px;"),
          tags$div(
            style = "display: flex; flex-direction: column; gap: 10px; margin-left: 10px;",
            tags$div(style = "border-left: 4px solid #10b981; padding-left: 12px;", "El subnodo alcanza un estado completamente puro (todos sus individuos pertenecen ya a la misma clase de \\(Y\\))."),
            tags$div(style = "border-left: 4px solid #3b82f6; padding-left: 12px;", "Se agotan los atributos predictivos disponibles en la muestra para continuar segmentando."),
            tags$div(style = "border-left: 4px solid #f59e0b; padding-left: 12px;", "Se alcanzan los límites prefijados por el investigador, tales como la profundidad máxima permitida o el número mínimo de observaciones por nodo.")
          )
        )
      ),
      
      br(),
      
      # =====================================
      # PODA (PRUNING)
      # =====================================
      bslib::card(
        style = "border: 1px solid #cbd5e1; background: #f8fafc;",
        bslib::card_body(
          h4(icon("scissors"), "Técnicas de Poda (Pruning) contra el Sobreajuste", style = "color: #1e40af; margin-bottom: 15px;"),
          p("Permitir que el árbol se ramifique sin restricciones suele inducir al sobreajuste (overfitting): el modelo memoriza el ruido de entrenamiento, mermando su capacidad de generalizar ante nuevos individuos (Han et al., 2011). Para mitigar este problema se aplican dos aproximaciones:"),
          tags$ul(
            tags$li(tags$b("Poda previa (Pre-pruning):"), " Detiene el algoritmo de forma temprana antes de que la estructura gane una complejidad excesiva.", style = "margin-bottom: 6px;"),
            tags$li(tags$b("Poda posterior (Post-pruning):"), " Colapsa ramas y simplifica nodos una vez que el árbol ha alcanzado su máximo crecimiento.")
          ),
          
          hr(style = "border-top: 1px solid #cbd5e1; margin: 20px 0;"),
          
          tags$div(
            style = "border-left: 4px solid #ef4444; background: #fef2f2; padding: 12px; border-radius: 0 8px 8px 0;",
            tags$h5(tags$b("Criterio de Coste-Complejidad en CART:"), style = "color: #991b1b; margin-bottom: 8px;"),
            p("Busca un compromiso matemático óptimo que equilibre el error de clasificación con el número de hojas mediante la penalización por el parámetro \\(\\alpha\\):"),
            p("$$R_\\alpha(T) = R(T) + \\alpha |T|$$"),
            
            p(style = "font-size: 0.9rem; margin-bottom: 0;", 
              "Donde \\(R(T)\\) representa el error empírico del árbol \\(T\\), \\(|T|\\) es el conteo de nodos terminales y \\(\\alpha\\) es el factor regulador. Valores elevados de \\(\\alpha\\) barren la estructura en favor de modelos altamente interpretables y robustos.")
          ) 
        ) 
      ) 
    ) 
  ) 
} 

Arboles_Teoria_Server <- function(id){
  moduleServer(id, function(input, output, session){ })
}

# -------------------------------
# ANALISIS
# -------------------------------

Arboles_Analisis_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Título uniforme con estilos de la suite de análisis
    h3("Árboles de Decisión - Modelado Predictivo", 
       style = "color: #1a446c; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; font-weight: 600; margin-top: 40px; margin-bottom: 20px; border-bottom: 2px solid #f4f6f9; padding-bottom: 10px;"),
    
    fluidRow(
      #--------------------------------------------------
      # PANEL LATERAL (CONFIGURACIÓN REACTIVA)
      #--------------------------------------------------
      column(
        width = 3,
        wellPanel(
          h4("Configuración"),
          p("El modelo se recalcula de forma reactiva instantánea al alterar cualquier parámetro."),
          hr(),
          
          uiOutput(ns("target_ui")),
          br(),
          uiOutput(ns("predictors_ui")),
          br(),
          
          sliderInput(
            ns("maxdepth"),
            "Profundidad máxima del árbol:",
            min = 1,
            max = 5,
            value = 3,
            step = 1
          ),
          br(),
          
          selectInput(
            ns("palette"),
            "Paleta estática de nodos:",
            choices = c(
              "Azules" = "Blues",
              "Verdes" = "Greens",
              "Rojo-Amarillo-Verde" = "RdYlGn",
              "Púrpuras" = "Purples"
            ),
            selected = "RdYlGn"
          ),
          
          hr(),
          helpText(
            "Los árboles estructuran reglas jerárquicas maximizando la ganancia de información (entropía/Gini) o reduciendo la varianza."
          ),
          uiOutput(ns("ui_dl_tree")) 
        )
      ),
      
      #--------------------------------------------------
      # PANEL PRINCIPAL
      #--------------------------------------------------
      column(
        width = 9,
        tabsetPanel(
          id = ns("tabs_tree"),
          
          #================================================
          # 1. DATOS (BARRAS INDEPENDIENTES NATURALES)
          #================================================
          tabPanel(
            "Datos",
            value = "datos",
            br(),
            p("Información: El algoritmo particiona los datos de forma iterativa y maneja de forma nativa tanto regresiones como clasificaciones.", 
              style = "color: #7f8c8d; font-style: italic; margin-bottom: 25px;"),
            
            h4("Dataset original", style = "color: #2c3e50; font-weight: 500;"),
            DT::DTOutput(ns("dataset")),
            
            br(), hr(), br(),
            
            h4("Dataset preparado / estandarizado", style = "color: #2c3e50; font-weight: 500;"),
            DT::DTOutput(ns("dataset_std"))
          ),
          
          #================================================
          # 2. ESTRUCTURA GRÁFICA DEL ÁRBOL
          #================================================
          tabPanel(
            "Árbol",
            value = "arbol",
            br(),
            wellPanel(p("Estructura de ramificación del modelo según las divisiones óptimas calculadas.")),
            plotOutput(ns("tree_plot"), height = "650px"),
            br(),
            h4("Interpretación del Grafo Jerárquico"),
            verbatimTextOutput(ns("interp_arbol"))
          ),
          
          #================================================
          # 3. IMPORTANCIA DE VARIABLES
          #================================================
          tabPanel(
            "Importancia de Variables",
            value = "importancia",
            br(),
            wellPanel(p("Porcentaje o magnitud relativa de contribución de cada predictor para disminuir la impureza global del árbol.")),
            plotOutput(ns("importance_plot"), height = "500px"),
            br(),
            h4("Análisis de Influencia"),
            verbatimTextOutput(ns("interp_importancia"))
          ),
          
          #================================================
          # 4. REGLAS TEXTUALES
          #================================================
          tabPanel(
            "Reglas de Decisión",
            value = "reglas",
            br(),
            wellPanel(p("Traducción algorítmica de las ramas en formato lógico condicional (IF-THEN).")),
            h4("Reglas de Extracción Lógica"),
            verbatimTextOutput(ns("tree_rules")),
            br(),
            h4("Conclusiones Operativas"),
            verbatimTextOutput(ns("interp_reglas"))
          )
        )
      )
    )
  )
}

# =============================================================================
# MODULO: ÁRBOLES DE DECISIÓN (RPART) - SERVER
# =============================================================================
Arboles_Analisis_Server <- function(id, datos, datos_ejemplo = NULL) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    filas_omitidas <- reactiveVal(0)
    
    #--------------------------------------------------
    # 1. TRATAMIENTO DE DATOS REACTIVOS
    #--------------------------------------------------
    datos_base <- reactive({
      df <- if (!is.null(datos()) && nrow(datos()) > 0) datos() else datos_ejemplo
      req(df)
      
      df[] <- lapply(df, function(x) {
        if (is.factor(x) || is.character(x)) {
          as_n <- suppressWarnings(as.numeric(as.character(x)))
          if (sum(!is.na(as_n)) > (length(x) * 0.8)) as_n else x
        } else {
          x
        }
      })
      
      filas_antes <- nrow(df)
      df_limpio <- na.omit(df)
      filas_despues <- nrow(df_limpio)
      filas_omitidas(filas_antes - filas_despues)
      
      df_limpio
    })
    
    #--------------------------------------------------
    # 2. SELECTORES INTERACTIVOS DIRECTOS
    #--------------------------------------------------
    output$target_ui <- renderUI({
      req(datos_base())
      selectInput(
        ns("target_var"),
        "Variable Objetivo (Y):",
        choices = names(datos_base()),
        selected = names(datos_base())[ncol(datos_base())]
      )
    })
    
    output$predictors_ui <- renderUI({
      req(input$target_var)
      opciones_x <- setdiff(names(datos_base()), input$target_var)
      
      selectizeInput(
        ns("predictor_vars"),
        "Variables Independientes (X):",
        choices = opciones_x,
        multiple = TRUE,
        selected = opciones_x[1:min(4, length(opciones_x))],
        options = list(plugins = list("remove_button"), persist = FALSE)
      )
    })
    
    #--------------------------------------------------
    # 3. GENERACIÓN DE DATASETS (REACTIVIDAD AUTOMÁTICA)
    #--------------------------------------------------
    datos_std <- reactive({
      req(input$target_var)
      df <- datos_base()
      num_cols <- sapply(df, is.numeric)
      num_cols[input$target_var] <- FALSE
      
      if (sum(num_cols) > 0) {
        df[, num_cols] <- scale(df[, num_cols, drop = FALSE])
      }
      df
    })
    
    # Renders de DT con scroll único y nativo (Hacia abajo y lateral)
    output$dataset <- DT::renderDT({
      req(datos_base())
      DT::datatable(
        datos_base(), 
        options = list(
          pageLength = 25, 
          scrollX = TRUE, 
          scrollY = "300px", 
          scrollCollapse = TRUE, 
          autoWidth = TRUE,
          dom = 'rtip'
        ),
        rownames = FALSE,
        style = "bootstrap"
      )
    })
    
    output$dataset_std <- DT::renderDT({
      req(datos_std())
      DT::datatable(
        datos_std(), 
        options = list(
          pageLength = 25, 
          scrollX = TRUE, 
          scrollY = "300px", 
          scrollCollapse = TRUE, 
          autoWidth = TRUE,
          dom = 'rtip'
        ),
        rownames = FALSE,
        style = "bootstrap"
      )
    })
    #--------------------------------------------------
    # 4. MOTOR DEL MODELO REACTIVO
    #--------------------------------------------------
    modelo_tree <- reactive({
      
      req(input$target_var)
      
      validate(
        need(length(input$predictor_vars) > 0,
             "Seleccione al menos una variable predictora.")
      )
      
      df <- datos_std()
      
      # Clasificación o regresión
      if (!is.numeric(df[[input$target_var]])) {
        df[[input$target_var]] <- as.factor(df[[input$target_var]])
      }
      
      formula_tree <- as.formula(
        paste(
          input$target_var,
          "~",
          paste(input$predictor_vars, collapse = " + ")
        )
      )
      
      metodo <- if (
        is.factor(df[[input$target_var]])
      ) "class" else "anova"
      
      rpart::rpart(
        formula = formula_tree,
        data = df,
        method = metodo,
        control = rpart::rpart.control(
          maxdepth = input$maxdepth,
          cp = 0.01
        )
      )
    })
    
    #--------------------------------------------------
    # 5. GRAFICO DEL ÁRBOL
    #--------------------------------------------------
    output$tree_plot <- renderPlot({
      
      req(modelo_tree())
      
      mod <- modelo_tree()
      
      validate(
        need(
          nrow(mod$frame) > 1,
          "El árbol generado contiene únicamente el nodo raíz."
        )
      )
      
      extra_dinamico <- ifelse(mod$method == "class", 104, 1)
      
      rpart.plot::rpart.plot(
        mod,
        type = 4,
        extra = extra_dinamico,
        under = TRUE,
        box.palette = input$palette,
        shadow.col = "#E0E0E0",
        nn = TRUE,
        fallen.leaves = TRUE,
        branch.lty = 3,
        main = paste(
          "Estructura de Decisiones para:",
          input$target_var
        )
      )
    })
    
    #--------------------------------------------------
    # 6. INTERPRETACIÓN DEL ÁRBOL
    #--------------------------------------------------
    output$interp_arbol <- renderText({
      
      req(modelo_tree())
      
      mod <- modelo_tree()
      
      n_nodos <- nrow(mod$frame)
      profundidad <- input$maxdepth
      
      paste0(
        "INTERPRETACIÓN DEL ÁRBOL:\n\n",
        "El modelo ha generado una estructura jerárquica con ",
        n_nodos,
        " nodos.\n\n",
        "Cada bifurcación representa una regla de decisión que maximiza la separación entre grupos mediante reducción de impureza.\n\n",
        "La profundidad máxima permitida fue de ",
        profundidad,
        ", lo que controla la complejidad y evita sobreajuste excesivo."
      )
    })
    
    #--------------------------------------------------
    # 7. IMPORTANCIA DE VARIABLES
    #--------------------------------------------------
    output$importance_plot <- renderPlot({
      
      req(modelo_tree())
      
      imp <- modelo_tree()$variable.importance
      
      validate(
        need(
          !is.null(imp),
          "No fue posible calcular la importancia de variables."
        )
      )
      
      imp <- sort(imp, decreasing = TRUE)
      
      barplot(
        imp,
        horiz = TRUE,
        las = 1,
        col = "steelblue",
        border = NA,
        xlab = "Importancia",
        main = "Importancia de Variables"
      )
    })
    
    #--------------------------------------------------
    # 8. INTERPRETACIÓN IMPORTANCIA
    #--------------------------------------------------
    output$interp_importancia <- renderText({
      
      req(modelo_tree())
      
      imp <- modelo_tree()$variable.importance
      
      if (is.null(imp) || length(imp) == 0) {
        return(
          "No fue posible calcular la importancia de variables."
        )
      }
      
      imp <- sort(imp, decreasing = TRUE)
      
      top_var <- names(imp)[1]
      
      paste0(
        "ANÁLISIS DE INFLUENCIA:\n\n",
        "La variable con mayor contribución al modelo es '",
        top_var,
        "'.\n\n",
        "Su importancia relativa indica que participa en las divisiones más relevantes del árbol y explica una parte sustancial de la reducción de impureza.\n\n",
        "Las variables aparecen ordenadas según su capacidad predictiva dentro del conjunto seleccionado."
      )
    })
    
    #--------------------------------------------------
    # 9. REGLAS DEL ÁRBOL
    #--------------------------------------------------
    output$tree_rules <- renderText({
      
      req(modelo_tree())
      
      paste(
        capture.output(
          rpart.plot::rpart.rules(
            modelo_tree(),
            cover = TRUE,
            roundint = FALSE
          )
        ),
        collapse = "\n"
      )
    })
    
    #--------------------------------------------------
    # 10. INTERPRETACIÓN DE REGLAS
    #--------------------------------------------------
    output$interp_reglas <- renderText({
      
      req(modelo_tree())
      
      paste0(
        "ANÁLISIS DE REGLAS DE DECISIÓN:\n\n",
        "Cada regla representa una ruta completa desde el nodo raíz hasta un nodo terminal.\n\n",
        "Estas reglas pueden interpretarse como condiciones IF-THEN fácilmente auditables, permitiendo comprender exactamente por qué una observación es asignada a una categoría o valor predicho.\n\n",
        "A diferencia de modelos tipo caja negra, los árboles ofrecen trazabilidad completa del proceso de decisión."
      )
    })

    #--------------------------------------------------
    # MENSAJE DE FILAS OMITIDAS
    #--------------------------------------------------
    output$ui_dl_tree <- renderUI({
      
      req(datos_base())
      
      omitidas <- filas_omitidas()
      
      if (omitidas > 0) {
        
        p(
          icon("triangle-exclamation"),
          paste0(
            " Información: Se han omitido ",
            omitidas,
            " filas debido a valores faltantes (NA)."
          ),
          style = paste(
            "color:#d35400;",
            "font-weight:500;",
            "font-size:12px;",
            "margin-top:15px;",
            "background-color:#fdf2e9;",
            "padding:8px;",
            "border-left:3px solid #e67e22;",
            "border-radius:4px;"
          )
        )
        
      } else {
        
        NULL
        
      }
    })
})}
    
# -------------------------------
# AUTOEVALUACION
# -------------------------------

Arboles_Auto_UI <- function(id) {
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

Arboles_Auto_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
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
        texto = "¿Qué son los árboles de decisión?",
        opciones = c(
          "Modelos predictivos que generan reglas jerárquicas para clasificar o predecir",
          "Técnicas de reducción de dimensionalidad",
          "Métodos para calcular correlaciones",
          "Modelos basados exclusivamente en regresión lineal"
        ),
        correcta = "Modelos predictivos que generan reglas jerárquicas para clasificar o predecir"
      ),
      
      list(
        texto = "¿Cuándo se utilizan los árboles de decisión?",
        opciones = c(
          "Cuando se desea clasificar individuos o realizar predicciones",
          "Únicamente para calcular medias",
          "Solo para reducir variables",
          "Exclusivamente para análisis factorial"
        ),
        correcta = "Cuando se desea clasificar individuos o realizar predicciones"
      ),
      
      list(
        texto = "¿Qué utilizan los árboles de decisión para clasificar a los individuos?",
        opciones = c(
          "Reglas de decisión basadas en los atributos predictivos",
          "Únicamente la variable dependiente",
          "Correlaciones entre observaciones",
          "Promedios de cada grupo"
        ),
        correcta = "Reglas de decisión basadas en los atributos predictivos"
      ),
      
      list(
        texto = "¿Con qué tipo de variables pueden trabajar los árboles de decisión?",
        opciones = c(
          "Variables categóricas y continuas",
          "Solo variables categóricas",
          "Solo variables numéricas",
          "Únicamente variables binarias"
        ),
        correcta = "Variables categóricas y continuas"
      ),
      
      list(
        texto = "¿Qué puede indicar que las métricas de ajuste del modelo sean extremadamente altas?",
        opciones = c(
          "Posible sobreajuste del modelo",
          "Falta de información en los datos",
          "Error en la variable objetivo",
          "Ausencia de variables predictoras"
        ),
        correcta = "Posible sobreajuste del modelo"
      ),
      
      list(
        texto = "En el dataset Breast Cancer utilizado en la aplicación, ¿qué variable suele aparecer como una de las más representativas para clasificar tumores?",
        opciones = c(
          "Radius_mean",
          "ID",
          "Diagnóstico",
          "Número de observación"
        ),
        correcta = "Radius_mean"
      ),
      
      list(
        texto = "¿Qué representa el accuracy de un árbol de decisión?",
        opciones = c(
          "La proporción de observaciones clasificadas correctamente",
          "El número total de variables del modelo",
          "La profundidad máxima del árbol",
          "La cantidad de nodos terminales"
        ),
        correcta = "La proporción de observaciones clasificadas correctamente"
      ),
      
      list(
        texto = "¿Qué es el sobreajuste (overfitting)?",
        opciones = c(
          "Cuando el modelo aprende el ruido de los datos y pierde capacidad de generalización",
          "Cuando el árbol tiene pocos nodos",
          "Cuando se eliminan variables predictoras",
          "Cuando el accuracy es bajo"
        ),
        correcta = "Cuando el modelo aprende el ruido de los datos y pierde capacidad de generalización"
      ),
      
      list(
        texto = "¿Qué mide la importancia de variables en un árbol de decisión?",
        opciones = c(
          "La contribución de cada predictor a la reducción de impureza",
          "La media de cada variable",
          "La correlación entre observaciones",
          "La cantidad de datos faltantes"
        ),
        correcta = "La contribución de cada predictor a la reducción de impureza"
      ),
      
      list(
        texto = "¿Qué representa una hoja en un árbol de decisión?",
        opciones = c(
          "La clasificación o predicción final",
          "La variable más importante",
          "La raíz del árbol",
          "Un dato perdido"
        ),
        correcta = "La clasificación o predicción final"
      ),
      
      list(
        texto = "¿Qué criterio utiliza habitualmente el algoritmo CART para seleccionar divisiones?",
        opciones = c(
          "Índice de Gini",
          "Coeficiente de correlación",
          "Media aritmética",
          "Distancia euclídea"
        ),
        correcta = "Índice de Gini"
      ),
      
      list(
        texto = "¿Qué mide la entropía en un árbol de decisión?",
        opciones = c(
          "El grado de incertidumbre o desorden de un nodo",
          "La profundidad del árbol",
          "El número de observaciones",
          "La precisión global del modelo"
        ),
        correcta = "El grado de incertidumbre o desorden de un nodo"
      ),
      
      list(
        texto = "Según la interpretación mostrada en la aplicación, ¿qué significa que una variable aparezca en primer lugar en el gráfico de importancia?",
        opciones = c(
          "Que es la que más contribuye a las divisiones relevantes del árbol",
          "Que tiene más valores perdidos",
          "Que fue introducida primero en la base de datos",
          "Que es la variable objetivo"
        ),
        correcta = "Que es la que más contribuye a las divisiones relevantes del árbol"
      ),
      
      list(
        texto = "¿Qué representan las reglas de decisión mostradas en la pestaña de reglas?",
        opciones = c(
          "Condiciones lógicas IF-THEN desde la raíz hasta una hoja",
          "Correlaciones entre variables",
          "Promedios de cada grupo",
          "Valores faltantes detectados"
        ),
        correcta = "Condiciones lógicas IF-THEN desde la raíz hasta una hoja"
      ),
      
      list(
        texto = "¿Cuál es una de las principales ventajas de los árboles de decisión frente a modelos de caja negra?",
        opciones = c(
          "Permiten interpretar fácilmente el proceso de decisión",
          "Siempre tienen mayor precisión",
          "No requieren datos",
          "Eliminan automáticamente el ruido"
        ),
        correcta = "Permiten interpretar fácilmente el proceso de decisión"
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
    
    todas_preguntas <- reactive({
      c(preguntas_base, preguntas_usuario())
    })
    
    preguntas_ordenadas <- reactiveVal(NULL)
    
    observe({
      lista_enriquecida <- lapply(todas_preguntas(), function(p) {
        p$id_unico <- paste0("af_q_", gsub("[^a-zA-Z0-9]", "", p$texto))
        p
      })
      
      if (is.null(isolate(preguntas_ordenadas()))) {
        preguntas_ordenadas(sample(lista_enriquecida, min(10, length(lista_enriquecida))))
      }
    })
    
    observeEvent(input$shuffle, {
      lista_enriquecida <- lapply(todas_preguntas(), function(p) {
        p$id_unico <- paste0("af_q_", gsub("[^a-zA-Z0-9]", "", p$texto))
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
              feedback_ui <- div(
                class = "text-success mt-2 font-weight-bold",
                "✔️ ¡Correcto!"
              )
            } else {
              feedback_ui <- div(
                class = "text-danger mt-2",
                paste0("❌ Incorrecto. Respuesta correcta: ", correct)
              )
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
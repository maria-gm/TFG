# =====================================================
#   BIPLOT - MODULO 
# =====================================================
BIPLOT_Teoria_UI <- function(id) {
  ns <- NS(id)
  tagList(
    # Llamada necesaria para que carguen las fórmulas en toda la página
    withMathJax(),
    
    tags$div(
      style = "padding: 20px; background-color: #fcfdfe;",
      
      # Cabecera con estilo
      h2("Análisis Biplot", style = "font-weight: 800; color: #1a365d; margin-bottom: 5px;"),
      p("Visualización simultánea de individuos (filas) y variables (columnas)", 
        style = "color: #64748b; font-size: 1.1rem; margin-bottom: 30px;"),
      
      # Fila de tarjetas principales
      bslib::layout_column_wrap(
        width = 1/3, # Tres columnas
        heights_equal = "row",
        
        # Tarjeta 1: Fundamento
        bslib::card(
          bslib::card_header(tags$b("1. Fundamento Matemático"), style = "background: #e0e7ff;"),
          bslib::card_body(
            p("A partir de la SVD:"),
            p("$$Y = U \\Lambda V^T$$"),
            p("factorizamos en dos matrices:"),
            p("$$Y \\cong (U \\Lambda^s)(V \\Lambda^{1-s})^T = AB^T$$"),
            tags$ul(
              style = "margin-top: 15px;",
              tags$li("\\(A = U \\Lambda^s\\) (Individuos)", style = "margin-bottom: 8px;"),
              tags$li("\\(B = V \\Lambda^{1-s}\\) (Variables)")
            )
          )
        ),
        
        # Tarjeta 2: Interpretación
        bslib::card(
          bslib::card_header(tags$b("2. Geometría"), style = "background: #dcfce7;"),
          bslib::card_body(
            tags$div(
              style = "display: flex; flex-direction: column; gap: 12px;",
              tags$div(
                style = "border-left: 4px solid #22c55e; padding-left: 10px;",
                tags$b("Puntos:"), " Representan a los individuos"
              ),
              tags$div(
                style = "border-left: 4px solid #f59e0b; padding-left: 10px;",
                tags$b("Vectores:"), " Representan las variables"
              ),
              p("El ángulo entre vectores indica la correlación de las variables:"),
              tags$ul(
                style = "margin-top: 5px; padding-left: 20px;",
                tags$li(tags$b("Ángulo agudo (~0°):"), " Alta correlación positiva", style = "margin-bottom: 6px;"),
                tags$li(tags$b("Ángulo recto (90°):"), " Variables independientes", style = "margin-bottom: 6px;"),
                tags$li(tags$b("Ángulo llano (~180°):"), " Alta correlación negativa")
              )
            )
          )
        ),
        
        # Tarjeta 3: Escalamientos
        bslib::card(
          bslib::card_header(tags$b("3. Tipos de Escalamiento"), style = "background: #fef3c7;"),
          bslib::card_body(
            p("El valor del parámetro de escala \\(s\\) determina qué propiedades métricas se priorizan en el espacio dual:"),
            tags$ul(
              style = "margin-top: 15px;",
              tags$li(
                tags$b("JK (\\(s = 1\\)):"), 
                " Preserva óptimamente las distancias euclídeas entre los individuos.",
                style = "margin-bottom: 12px;"
              ),
              tags$li(
                tags$b("GH (\\(s = 0\\)):"), 
                " Preserva las varianzas y las estructuras de correlación entre variables.",
                style = "margin-bottom: 12px;"
              ),
              tags$li(
                tags$b("SQRT (\\(s = 1/2\\)):"), 
                " Reparto equilibrado de la inercia entre filas y columnas (Simétrico)."
              )
            )
          )
        )
      ), 
      
      br(),
      
      # Nota para el HJ Biplot
      bslib::card(
        style = "border: 1px solid #cbd5e1; background: #f8fafc;",
        full_screen = FALSE,
        bslib::card_body(
          h4(icon("star"), "Extensión: HJ Biplot", style = "color: #1e40af;"),
          p("Propuesto por Galindo (1986), busca que tanto filas como columnas tengan la máxima calidad de representación en un mismo espacio de baja dimensión.")
        )
      )
    )
  )
}

BIPLOT_Teoria_Server <- function(id){
  moduleServer(id, function(input, output, session){ })
}
#-------------------------------
# Análisis
# -------------------------------
BIPLOT_Analisis_UI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    h3("Análisis", 
       style = "color: #1a446c; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; font-weight: 600; margin-top: 40px; margin-bottom: 20px; border-bottom: 2px solid #f4f6f9; padding-bottom: 10px;"),
    
    fluidRow(
      #--------------------------------------------------
      # PANEL LATERAL PERMANENTE
      #--------------------------------------------------
      column(
        width = 3,
        wellPanel(
          h4("Configuración"),
          p("Parámetros de proyección para el Biplot."),
          
          selectInput(
            ns("tipo_biplot"),
            "Tipo de biplot:",
            choices = c(
              "HJ (Máxima calidad conjunta)" = "HJ",
              "GH (Prioriza variables)" = "GH", 
              "JK (Prioriza individuos)" = "JK", 
              "SQRT (Equilibrio de varianza)" = "SQRT"
            ),
            selected = "HJ"
          ),
          
          radioButtons(
            ns("tipo_vista"),
            "Visualización:",
            choices = c("2D", "3D"),
            inline = TRUE,
            selected = "2D"
          ),
          
          # Contenedor dinámico exclusivo para los checkboxes
          uiOutput(ns("dinamico_panel_lateral")),
          
          hr(),
          helpText("Los vectores representan las variables y los puntos corresponden a las observaciones."),
          br(),
          helpText("Nota: Se eliminan filas con valores faltantes automáticamente."),
        )
      ),
      
      #--------------------------------------------------
      # PANEL PRINCIPAL
      #--------------------------------------------------
      column(
        width = 9,
        tabsetPanel(
          id = ns("tabs_biplot"),
          
          tabPanel(
            "Datos", value = "datos",
            br(),
            uiOutput(ns("alerta_datos_biplot")),
            h4("Dataset original", style = "color: #2c3e50; font-weight: 500;"),
            DT::DTOutput(ns("dataset_table")),
            br(), hr(), br(),
            h4("Dataset estandarizado", style = "color: #2c3e50; font-weight: 500;"),
            DT::DTOutput(ns("dataset_std_table"))
          ),
          
          tabPanel(
            "Proyección Biplot", value = "proyeccion",
            br(),
            uiOutput(ns("alerta_proj_biplot")),
            wellPanel(p("Análisis visual simultáneo de filas (individuos) y columnas (variables).")),
            h4("Representación Gráfica Interactiva", style = "color: #2c3e50; font-weight: 500;"),
            plotlyOutput(ns("biplot_grafico"), height = "550px"),
            br(),
            h4("Interpretación", style = "color: #2c3e50; font-weight: 500;"),
            verbatimTextOutput(ns("interp_biplot"))
          )
        )
      )
    )
  )
}

BIPLOT_Analisis_Server <- function(id, datos, datos_ejemplo = NULL){
  
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
    # ==========================================================================
    # FUNCIONES LOCALES DE VALIDACIÓN (Encapsuladas en el Server)
    # ==========================================================================
    validar_datos_multivar <- function(df, min_vars = 2, min_obs = 3) {
      if (is.null(df)) return("No se han cargado datos.")
      df_num <- df[, sapply(df, is.numeric), drop = FALSE]
      if (ncol(df_num) < min_vars) {
        return(paste0("El dataset no tiene suficientes variables numéricas (mínimo requerido: ", min_vars, ")."))
      }
      df_clean <- df_num[complete.cases(df_num), ]
      if (nrow(df_clean) < min_obs) {
        return(paste0("No hay suficientes observaciones completas (sin valores ausentes) para el análisis (mínimo requerido: ", min_obs, ")."))
      }
      return(NULL)
    }
    
    preprocesar_datos <- function(df) {
      df_num <- df[, sapply(df, is.numeric), drop = FALSE]
      df_clean <- df_num[complete.cases(df_num), ]
      as.matrix(scale(df_clean, center = TRUE, scale = TRUE))
    }
 
    mensaje_error_analisis <- function(mensaje) {
      shiny::tags$div(
        class = "alert alert-danger",
        style = "background-color: #f2dede; color: #a94442; border-color: #ebccd1; padding: 15px; margin-bottom: 20px; border: 1px solid transparent; border-radius: 4px; font-family: inherit;",
        tags$b(icon("triangle-exclamation"), " No es posible realizar el análisis."),
        br(),
        mensaje
      )
    }
    
    # ==========================================================================
    # LÓGICA REACTIVA DEL MÓDULO
    # ==========================================================================
    
    # 1. Evaluación de Calidad
    evaluacion_biplot <- reactive({
      df <- if(!is.null(datos()) && nrow(datos()) > 0) datos() else datos_ejemplo
      error_msg <- validar_datos_multivar(df, min_vars = 2, min_obs = 3)
      if (!is.null(error_msg)) return(list(valido = FALSE, mensaje = error_msg))
      
      df_num <- df[, sapply(df, is.numeric), drop = FALSE]
      df_clean <- df_num[complete.cases(df_num), ]
      
      var_ok <- apply(df_clean, 2, var) > 0
      if (sum(var_ok) < 2) return(list(valido = FALSE, mensaje = "Se requieren al menos 2 variables con varianza mayor a cero."))
      
      return(list(valido = TRUE, datos = df_clean[, var_ok, drop = FALSE]))
    })
    
    # Gestión de Banners de Alerta
    output$alerta_datos_biplot <- renderUI({
      eval <- evaluacion_biplot()
      if (!eval$valido) mensaje_error_analisis(eval$mensaje) else NULL
    })
    output$alerta_proj_biplot <- renderUI({
      eval <- evaluacion_biplot()
      if (!eval$valido) mensaje_error_analisis(eval$mensaje) else NULL
    })
    
    # 2. Renderizado del Selector de Variables en el Panel Lateral
    output$dinamico_panel_lateral <- renderUI({
      req(evaluacion_biplot()$valido)
      columnas <- names(evaluacion_biplot()$datos)
      
      tagList(
        hr(),
        checkboxGroupInput(
          ns("vars_seleccionadas"),
          label = "Variables para el análisis:",
          choices = columnas,
          selected = columnas
        )
      )
    })
    
    # 3. Preparación de Matrices de Datos
    datos_std <- reactive({
      req(evaluacion_biplot()$valido, input$vars_seleccionadas)
      req(length(input$vars_seleccionadas) >= 2)
      
      df_original <- if(!is.null(datos()) && nrow(datos()) > 0) datos() else datos_ejemplo
      matriz_std <- preprocesar_datos(df_original)
      
      matriz_std[, input$vars_seleccionadas, drop = FALSE]
    })
    
    # 4. Descomposición SVD y Coordenadas del Biplot
    coords <- reactive({
      req(evaluacion_biplot()$valido)
      req(datos_std(), input$tipo_vista, input$tipo_biplot)
      
      s <- svd(datos_std())
      k <- ifelse(input$tipo_vista == "3D", 3, 2)
      if (length(s$d) < k) k <- length(s$d)
      req(k >= 2)
      
      U <- s$u[, 1:k, drop = FALSE]
      V <- s$v[, 1:k, drop = FALSE]
      D <- diag(s$d[1:k], nrow = k)
      D_sqrt <- diag(sqrt(s$d[1:k]), nrow = k)
      
      A <- switch(input$tipo_biplot,
                  "GH" = U,
                  "JK" = U %*% D,
                  "SQRT" = U %*% D_sqrt,
                  U %*% D) # HJ por defecto
      
      B <- switch(input$tipo_biplot,
                  "GH" = V %*% D,
                  "JK" = V,
                  "SQRT" = V %*% D_sqrt,
                  V %*% D)
      
      colnames(A) <- paste0("Dim", 1:k)
      colnames(B) <- paste0("Dim", 1:k)
      rownames(A) <- rownames(datos_std())
      rownames(B) <- colnames(datos_std())
      
      list(A = A, B = B, k = k)
    })
    
    # 5. Renderizado de Tablas
    output$dataset_table <- DT::renderDT({
      req(evaluacion_biplot()$valido)
      # Usamos el dataframe numérico y limpio global para que coincida 100% con el gráfico
      df_mostrado <- evaluacion_biplot()$datos 
      
      DT::datatable(df_mostrado, 
                    options = list(paging = FALSE, scrollY = "400px", scrollX = TRUE))
    })
    output$dataset_std_table <- DT::renderDT({
      req(evaluacion_biplot()$valido)
      req(datos_std())
      DT::datatable(round(as.data.frame(datos_std()), 3), 
                    options = list(paging = FALSE, scrollY = "400px", scrollX = TRUE))
    })
    
    # 6. Renderizado del Gráfico Interactivo (Plotly) Corregido
    output$biplot_grafico <- renderPlotly({
      req(evaluacion_biplot()$valido)
      req(coords())
      c <- coords()
      A <- as.data.frame(c$A)
      B <- as.data.frame(c$B)
      
      if(c$k == 2) {
        # Proyección 2D
        p <- plot_ly() %>%
          add_markers(data = A, x = ~Dim1, y = ~Dim2, text = rownames(A), 
                      marker = list(color = '#2c3e50', size = 6), name = "Individuos", hoverinfo = "text")
        
        for(i in 1:nrow(B)) {
          p <- p %>% add_segments(x = 0, y = 0, xend = B$Dim1[i], yend = B$Dim2[i], 
                                  line = list(color = '#e74c3c', width = 2), showlegend = FALSE)
        }
        
        p %>% add_text(data = B, x = ~Dim1*1.1, y = ~Dim2*1.1, text = rownames(B), 
                       textfont = list(color = '#e74c3c', size = 12, weight = 'bold'), name = "Variables") %>%
          layout(xaxis = list(title = "Dimensión 1", zeroline = TRUE),
                 yaxis = list(title = "Dimensión 2", zeroline = TRUE))
      } else {
        # Proyección 3D
        p <- plot_ly() %>%
          add_markers(data = A, x = ~Dim1, y = ~Dim2, z = ~Dim3, text = rownames(A), 
                      marker = list(size = 3, color = '#2c3e50'), name = "Individuos", hoverinfo = "text")
        
        for(i in 1:nrow(B)) {
          p <- p %>% add_trace(x = c(0, B$Dim1[i]), y = c(0, B$Dim2[i]), z = c(0, B$Dim3[i]), 
                               type = "scatter3d", mode = "lines", line = list(color = '#e74c3c', width = 3), showlegend = FALSE)
        }
        
        p %>% add_text(data = B, x = ~Dim1*1.1, y = ~Dim2*1.1, z = ~Dim3*1.1, text = rownames(B), 
                       textfont = list(color = '#e74c3c', size = 11, weight = 'bold'), name = "Variables") %>%
          layout(scene = list(xaxis = list(title = "Dimensión 1"),
                              yaxis = list(title = "Dimensión 2"),
                              zaxis = list(title = "Dimensión 3")))
      }
    })
    
    #--------------------------------------------------
    # INTERPRETACIÓN DINÁMICA DE TIPOS DE BIPLOT (BASADA EN WINES)
    #--------------------------------------------------
    output$interp_biplot <- renderText({
      req(input$tipo_biplot)
      tipo <- input$tipo_biplot
      
      # 1. Reglas básicas comunes (Siempre se muestran arriba)
      msg_base <- paste0(
        "Configuración activa: Biplot modelo ", tipo, ".\n",
        "Reglas de interpretación general:\n",
        "- Un ángulo agudo entre vectores indica correlación positiva fuerte entre las variables.\n",
        "- Un ángulo recto (90°) denota independencia lineal.\n",
        "- La proximidad de un punto (individuo) a la dirección de un vector señala un valor alto en dicha propiedad.\n\n"
      )
      
      # 2. Explicación teórica y aplicación práctica unificada por cada tipo de Biplot
      msg_tipo <- switch(tipo,
                         "HJ" = paste0(
                           "Guía del modelo HJ Biplot (Galindo, 1986):\n",
                           "Este método aplica una escala que maximiza simultáneamente la calidad de representación de los ",
                           "individuos (filas) y de las variables (columnas) en el mismo espacio vectorial. Es ideal para análisis ",
                           "exploratorios generales ya que no sesga la interpretación hacia un elemento en detrimento del otro.\n\n",
                           "Ejemplo práctico (Dataset 'wines'): Al utilizar el modelo HJ, visualizarás de forma óptima tanto la ",
                           "estructura de las variables como de las observaciones. Notarás cómo las variables 'Flavanoids' y 'Total Phenols' ",
                           "proyectan vectores muy cercanos y paralelos (alta correlación), mientras que las muestras pertenecientes ",
                           "al cultivo de vino 1 se sitúan en la dirección extrema de dichos vectores, delatando su alta composición polifenólica."
                         ),
                         "GH" = paste0(
                           "Guía del modelo GH Biplot (Asimétrico - Enfoque en Variables):\n",
                           "Prioriza la visualización de las columnas (variables). En este modelo, las longitudes de los vectores ",
                           "y los ángulos entre ellos reflejan de forma óptima las varianzas, covarianzas y correlaciones reales ",
                           "del conjunto de datos, sacrificando precisión en las distancias euclídeas entre individuos.\n\n",
                           "Ejemplo práctico (Dataset 'wines'): Si utilizas el modelo GH, concéntrate en la geometría de las variables ",
                           "químicas. Las longitudes de los vectores serán proporcionales a su variabilidad real. Observarás con total ",
                           "precisión matemática que el ángulo de casi 90° entre las flechas de 'Alcohol' y 'Malic Acid' corrobora ",
                           "que ambas propiedades actúan de forma linealmente independiente en el perfil químico de los vinos."
                         ),
                         "JK" = paste0(
                           "Guía del modelo JK Biplot (Asimétrico - Enfoque en Individuos):\n",
                           "Prioriza la estructura de las filas (individuos). Las distancias geométricas entre los puntos del gráfico ",
                           "respetan de manera fidedigna las distancias euclídeas de los sujetos en el espacio multidimensional original, ",
                           "por lo que es excelente para detectar anomalías, agrupamientos (clusters) u observaciones atípicas.\n\n",
                           "Ejemplo práctico (Dataset 'wines'): Bajo el enfoque del modelo JK, la prioridad absoluta es la clasificación. ",
                           "Verás cómo las 178 muestras de vino se agrupan de forma impecable y nítida en 3 clusters espaciales compactos ",
                           "que delimitan los tres tipos de uva italiana originales, aunque la escala visual de los vectores químicos ",
                           "(como 'Ash' o 'Magnesium') quede en un segundo plano interpretativo."
                         ),
                         "SQRT" = paste0(
                           "Guía del modelo SQRT Biplot (Simétrico):\n",
                           "Distribuye los valores singulares (varianza) equitativamente asignando la raíz cuadrada a las coordenadas ",
                           "de las filas y de las columnas de forma matemática pura. Ofrece una aproximación intermedia y equilibrada.\n\n",
                           "Ejemplo práctico (Dataset 'wines'): El modelo SQRT balancea las escalas para pantallas estándar. Evita que las ",
                           "flechas de variables con mucha varianza (como 'Color Intensity') se disparen hacia los extremos saliéndose ",
                           "del recuadro, distribuyendo armónicamente tanto los vectores químicos como la dispersión de los 178 vinos."
                         )
      )
      
      return(paste0(msg_base, msg_tipo))
    })
    
  }) 
} 
# -------------------------------
# AUTOEVALUACION
# -------------------------------
BIPLOT_Auto_UI <- function(id) {
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
        title = "➕ Gestión: Añadir pregunta personalizada del Biplot",
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
        
        actionButton(ns("add"), "Guardar pregunta en el banco del Biplot", class = "btn-success btn-sm mt-2")
      )
    )
  )
}

BIPLOT_Auto_Server <- function(id) {
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
      list(texto = "¿Qué representa un biplot?", opciones = c("Solo individuos", "Solo variables", "Individuos y variables simultáneamente", "Matriz de correlaciones"), correcta = "Individuos y variables simultáneamente"),
      list(texto = "¿Cuál es el objetivo principal de los biplots?", opciones = c("Representar gráficamente individuos y variables en un mismo plano", "Calcular medias muestrales", "Eliminar la multicolinealidad", "Clasificar observaciones"), correcta = "Representar gráficamente individuos y variables en un mismo plano"),
      list(texto = "¿Qué caracteriza a los HJ-Biplots?", opciones = c("Representación solo de variables", "Igual calidad de representación para filas y columnas", "Uso exclusivo en regresión logística", "No utilizan descomposición SVD"), correcta = "Igual calidad de representación para filas y columnas"),
      list(texto = "¿Qué es la bondad de ajuste en un biplot?", opciones = c("El porcentaje de varianza explicada por el plano", "El número de variables eliminadas", "El error de predicción del modelo", "El número de clusters obtenidos"), correcta = "El porcentaje de varianza explicada por el plano"),
      list(texto = "¿Qué representa la proximidad entre individuos en un biplot?", opciones = c("Diferencia en escalas", "Independencia estadística", "Similitud entre observaciones", "Correlación entre variables"), correcta = "Similitud entre observaciones"),
      list(texto = "¿Qué tipo de representación preserva mejor las distancias entre individuos?", opciones = c("Representación de variables", "PCA sin estandarizar", "Representación centrada solo en variables", "Representación de individuos (fila-métrica)"), correcta = "Representación de individuos (fila-métrica)"),
      list(texto = "¿Qué significa la longitud de una variable en el biplot?", opciones = c("Su varianza o contribución al plano", "El número de observaciones", "El número de componentes", "El error estándar"), correcta = "Su varianza o contribución al plano"),
      list(texto = "¿Qué información se pierde al proyectar en 2D?", opciones = c("Ninguna, se conserva toda la información", "Solo la media de las variables", "Parte de la varianza original", "Las etiquetas de los individuos"), correcta = "Parte de la varianza original"),
      list(texto = "¿Qué ocurre si dos variables forman un ángulo pequeño en un biplot?", opciones = c("Son independientes", "Están positivamente correlacionadas", "Están negativamente correlacionadas", "No tienen interpretación"), correcta = "Están positivamente correlacionadas"),
      list(texto = "¿Qué indica un ángulo de 90 grados entre dos variables en el biplot?", opciones = c("Correlación máxima", "Que son linealmente independientes", "Que miden lo mismo", "Error de cálculo"), correcta = "Que son linealmente independientes"),
      list(texto = "Si un individuo se proyecta muy cerca del origen, ¿qué significa?", opciones = c("Tiene valores extremos", "Está mal representado o cercano a la media", "Es un outlier", "No pertenece al dataset"), correcta = "Está mal representado o cercano a la media"),
      list(texto = "¿Qué eje del biplot suele explicar más información?", opciones = c("Eje vertical", "Eje horizontal", "Ambos por igual", "Tercer eje"), correcta = "Eje horizontal"),
      list(texto = "¿En qué consiste la calidad de representación de un punto?", opciones = c("Color del punto", "Qué tan bien refleja el plano 2D la estructura real", "Tamaño de etiqueta", "Escala de ejes"), correcta = "Qué tan bien refleja el plano 2D la estructura real"),
      list(texto = "Al observar un HJ-Biplot del dataset, el vector químico 'Ash' (cenizas) es extremadamente corto en comparación con 'Alcohol'. ¿Qué conclusión matemática extrae?", opciones = c("La variable 'Ash' no tiene variabilidad real en el dataset", "La variable 'Ash' está mal representada en este plano biplot 2D (baja calidad de representación)", "El alcohol tiene menos impacto en la varianza estructural que las cenizas", "La medición de las cenizas se introdujo incorrectamente"), correcta = "La variable 'Ash' está mal representada en este plano biplot 2D (baja calidad de representación)"),
      list(texto = "Si el vino marcado con la etiqueta 'Observation 46' se ubica exactamente sobre la punta del vector de la variable 'Magnesium', ¿qué podemos asegurar con certeza sobre esa botella?", opciones = c("Que es el vino con menos magnesio de la bodega", "Que tiene un valor sustancialmente superior a la media en contenido de magnesio", "Que es un valor atípico que arruina el ajuste global", "Que su valor de magnesio es exactamente cero"), correcta = "Que tiene un valor sustancialmente superior a la media en contenido de magnesio")
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
    
    # muestra aleatoria inicial de  10 preguntas
    observe({
      lista_enriquecida <- lapply(seq_along(todas_preguntas()), function(idx) {
        p <- todas_preguntas()[[idx]]
        p$id_unico <- paste0("bp_q_", gsub("[^a-zA-Z0-9]", "", p$texto))
        p
      })
      
      # Tomamos 10 aleatorias del total disponible
      n_mostrar <- min(10, length(lista_enriquecida))
      muestra_inicial <- sample(lista_enriquecida, n_mostrar)
      preguntas_ordenadas(muestra_inicial)
    })
    
    # Al presionar el botón, selecciona 10 preguntas NUEVAS del banco total y mezcla sus opciones
    observeEvent(input$shuffle, {
      lista_enriquecida <- lapply(seq_along(todas_preguntas()), function(idx) {
        p <- todas_preguntas()[[idx]]
        p$id_unico <- paste0("bp_q_", gsub("[^a-zA-Z0-9]", "", p$texto))
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
    output$preguntas <- renderUI({
      req(preguntas_ordenadas())
      
      tagList(
        lapply(seq_along(preguntas_ordenadas()), function(i) {
          
          pregunta_actual <- preguntas_ordenadas()[[i]]
          id_input <- pregunta_actual$id_unico
          
          feedback_ui <- NULL
          
          if (isTRUE(mostrar_respuestas())) {
            
            user_ans <- input[[id_input]]
            correct_ans <- pregunta_actual$correcta
            
            if (!is.null(user_ans) && user_ans == correct_ans) {
              feedback_ui <- div(
                class = "text-success mt-2 font-weight-bold",
                "✔️ ¡Correcto!"
              )
            } else {
              feedback_ui <- div(
                class = "text-danger mt-2",
                paste0("❌ Incorrecto. Respuesta correcta: ", correct_ans)
              )
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

    
    output$score <- renderUI({
      req(mostrar_respuestas())
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

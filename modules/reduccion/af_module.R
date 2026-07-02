
# =====================================================
#   ANALISIS FACTORIAL 
# =====================================================
# =====================================================
#   TEORÍA
# =====================================================
AF_Teoria_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # activar MathJax
    withMathJax(),
    
    tags$div(
      style = "padding: 20px; background-color: #fcfdfe;",
      
      # =================================================
      # CABECERA
      # =================================================
      h2(
        "Análisis Factorial (AF)",
        style = "font-weight: 800; color: #1a365d; margin-bottom: 5px;"
      ),
      
      p(
        "Técnica multivariante orientada a reducir la dimensionalidad e identificar factores latentes.",
        style = "color: #64748b; font-size: 1.1rem; margin-bottom: 30px;"
      ),
      
      # =================================================
      # TARJETAS PRINCIPALES
      # =================================================
      bslib::layout_column_wrap(
        width = 1/3, # Tres columnas 
        heights_equal = "row",
        
        # =============================================
        # FUNDAMENTO
        # =============================================
        bslib::card(
          bslib::card_header(
            tags$b("1. Modelo factorial"),
            style = "background: #e0e7ff;"
          ),
          bslib::card_body(
            p("El modelo factorial expresa las variables observadas en función de los factores comunes y los factores únicos o específicos:"),
            p("$$\\mathbf{X} = \\mathbf{AF} + \\mathbf{DU}$$"),
            tags$ul(
              style = "margin-top: 10px;",
              tags$li("\\(\\mathbf{X}\\) : Matriz de datos de las variables observadas.", style = "margin-bottom: 6px;"),
              tags$li("\\(\\mathbf{A}\\) : Matriz de cargas factoriales (loadings).", style = "margin-bottom: 6px;"),
              tags$li("\\(\\mathbf{F}\\) : Matriz de los factores comunes.", style = "margin-bottom: 6px;"),
              tags$li("\\(\\mathbf{D}\\) : Matriz de coeficientes de los factores únicos.", style = "margin-bottom: 6px;"),
              tags$li("\\(\\mathbf{U}\\) : Matriz de los factores únicos o específicos.")
            )
          )
        ),
        
        # =============================================
        # INTERPRETACIÓN
        # =============================================
        bslib::card(
          bslib::card_header(
            tags$b("2. Interpretación"),
            style = "background: #dcfce7;"
          ),
          bslib::card_body(
            p("La evaluación de los resultados se fundamenta en tres pilares prácticos del espacio factorial:"),
            tags$div(
              style = "display: flex; flex-direction: column; gap: 12px; margin-top: 15px;",
              tags$div(
                style = "border-left: 4px solid #22c55e; padding-left: 10px;",
                tags$b("Cargas altas:"), " Indican una fuerte relación lineal o asociación entre la variable original y el factor."
              ),
              tags$div(
                style = "border-left: 4px solid #f59e0b; padding-left: 10px;",
                tags$b("Comunalidades:"), " Miden la proporción de varianza de cada variable que es explicada por los factores comunes."
              ),
              tags$div(
                style = "border-left: 4px solid #3b82f6; padding-left: 10px;",
                tags$b("Factores:"), " Representan constructos o dimensiones latentes subyacentes que no son directamente observables."
              )
            )
          )
        ),
        
        # =============================================
        # OBJETIVOS
        # =============================================
        bslib::card(
          bslib::card_header(
            tags$b("3. Objetivos"),
            style = "background: #fef3c7;"
          ),
          bslib::card_body(
            p("Esta metodología busca simplificar matrices de datos complejas persiguiendo:"),
            tags$ul(
              style = "margin-top: 15px;",
              tags$li("Reducir el número de variables reduciendo la redundancia.", style = "margin-bottom: 10px;"),
              tags$li("Detectar estructuras y dimensiones subyacentes en los datos.", style = "margin-bottom: 10px;"),
              tags$li("Identificar grupos homogéneos de variables relacionadas.", style = "margin-bottom: 10px;"),
              tags$li("Facilitar la interpretación teórica de los fenómenos analizados.")
            )
          )
        )
      ),
      
      br(),
      
      # =================================================
      # MÉTODOS EXTRACCIÓN (TFG RESUMIDO)
      # =================================================
      bslib::card(
        style = "border: 1px solid #cbd5e1; background: #f8fafc;",
        bslib::card_body(
          h4(
            icon("gear"), "Estimación de factores y Ejes Principales",
            style = "color: #1e40af; margin-bottom: 10px;"
          ),
          p("El objetivo es estimar la matriz de cargas factoriales \\(\\mathbf{A}\\) a partir de la matriz de correlaciones reducida \\(\\mathbf{R}^*\\) (López-Roldán & Fachelli, 2016), donde los elementos de la diagonal se sustituyen por comunalidades estimadas: \\(r_{ii} = h_i^2\\). Basado en el teorema de Thurstone, se cumple que:"),
          p("$$\\mathbf{R}^* = \\mathbf{A}\\mathbf{A}^T$$"),
          
          p(tags$b("Algoritmo Iterativo de los Ejes Principales (Cuadras, 2014):")),
          tags$ul(
            style = "margin-top: 10px;",
            tags$li(
              tags$b("Paso 0 (Inicialización):"), 
              " Se estiman las comunalidades iniciales (vía MAC o SMC) para formar \\(\\mathbf{R}^*\\) y se obtiene su descomposición espectral: \\(\\mathbf{R}^* = \\mathbf{V}\\mathbf{\\Lambda}\\mathbf{V}^T\\).",
              style = "margin-bottom: 8px;"
            ),
            tags$li(
              tags$b("Paso 1 (Primera aproximación):"), 
              " Se calcula la matriz de cargas reducida con los \\(k\\) primeros factores: \\(\\mathbf{A}_1 = \\mathbf{V}_k\\mathbf{\\Lambda}_k^{1/2}\\). Se actualizan las comunalidades en la diagonal de la nueva matriz reducida: \\(\\mathbf{R}_1^* = \\text{diag}(\\mathbf{A}_1\\mathbf{A}_1^T) + \\mathbf{R} - \\mathbf{I}\\).",
              style = "margin-bottom: 8px;"
            ),
            tags$li(
              tags$b("Paso i (Iteración y convergencia):"), 
              " El proceso se repite sucesivamente calculando la descomposición espectral de \\(\\mathbf{R}_i^*\\) hasta alcanzar la condición de parada: cuando las comunalidades calculadas convergen y dejen de variar significativamente respecto a la iteración previa (\\(\\mathbf{A}_i \\approx \\mathbf{A}_{i-1}\\))."
            )
          )
        )
      ),
      
      
      br(),
      
      # =================================================
      # ROTACIÓN
      # =================================================
      bslib::card(
        style = "border: 1px solid #cbd5e1; background: #f8fafc;",
        bslib::card_body(
          h4(
            icon("rotate"), "Rotación factorial",
            style = "color: #1e40af; margin-bottom: 10px;"
          ),
          p("Dado que la solución inicial no es única, la rotación transforma la matriz original de cargas factoriales para aproximarse a la estructura simple de interpretación:"),
          p("$$\\mathbf{B} = \\mathbf{AT}$$"),
          tags$ul(
            style = "margin-top: 10px;",
            tags$li(tags$b("Varimax (Ortogonal):"), " Mantiene los factores completamente perpendiculares e independientes entre sí, optimizando la interpretación al aproximar las cargas a valores extremos (cercanos a 0 o 1).")
          )
        )
      ),
      
      br(),
      
      # =================================================
      # CRITERIOS
      # =================================================
      bslib::card(
        style = "border: 1px solid #cbd5e1; background: #f8fafc;",
        bslib::card_body(
          h4(
            icon("arrow-down-short-wide"), "Selección del número de factores",
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
      )
    )
  )
}

AF_Teoria_Server <- function(id){
  
  moduleServer(id, function(input, output, session){
    
  })
  
}

# =====================================================
#   ANALISIS  
# =====================================================
AF_Analisis_UI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    
    # Título 
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
          
          p("Seleccione los parámetros para estructurar la retención y rotación de los factores."),
          
          hr(),
          
          uiOutput(ns("panel_lateral_af")),
          
          hr(),
          
          helpText(
            "Nota: La extracción se realiza mediante el métodos de los Ejes Principales (pa)."
          ),
          
          helpText(
            "Las cargas factoriales indican la relación entre las variables y los factores extraídos."
          ),
          
          br(),
          
          uiOutput(ns("ui_dl_af")),
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
          id = ns("tabs_af"), 
          
          #================================================
          # DATOS
          #================================================
          tabPanel(
            "Datos",
            value = "datos",
            
            br(),
            uiOutput(ns("alerta_datos_af")), # Alerta local inyectada sin romper la UI
            
            p("Información: El análisis se ejecuta exclusivamente sobre las columnas de tipo numérico.", 
              style = "color: #7f8c8d; font-style: italic; margin-bottom: 25px;"),
            
            h4("Dataset original", style = "color: #2c3e50; font-weight: 500;"),
            DT::DTOutput(ns("dataset_af")),
            
            br(), hr(), br(),
            
            h4("Dataset estandarizado", style = "color: #2c3e50; font-weight: 500;"),
            DT::DTOutput(ns("dataset_std_af"))
          ),
          
          #================================================
          # DIAGNÓSTICO
          #================================================
          tabPanel(
            "Diagnóstico",
            value = "diagnostico",
            
            br(),
            uiOutput(ns("alerta_diag_af")), # Alerta local inyectada sin romper la UI
            
            wellPanel(
              p("Evaluación de la adecuación muestral para el Análisis Factorial.")
            ),
            
            h4("Test de Bartlett", style = "color: #2c3e50; font-weight: 500;"),
            verbatimTextOutput(ns("bartlett_test_af")),
            
            br(),
            
            h4("Test KMO (Kaiser-Meyer-Olkin)", style = "color: #2c3e50; font-weight: 500;"),
            verbatimTextOutput(ns("kmo_test")),
            
            br(),
            
            h4("Matriz de correlaciones (Heatmap)", style = "color: #2c3e50; font-weight: 500;"),
            plotOutput(ns("corrplot_af"), height = "500px")
          ),
          
          #================================================
          # SELECCIÓN DE FACTORES
          #================================================
          tabPanel(
            "Selección de Factores",
            value = "componentes",
            
            br(),
            uiOutput(ns("alerta_scree_af")), # Alerta local inyectada sin romper la UI
            
            wellPanel(
              p("Scree plot y varianza común explicada.")
            ),
            
            h4("Scree Plot", style = "color: #2c3e50; font-weight: 500;"),
            plotOutput(ns("scree_af"), height = "450px"),
            
            br(),
            
            h4("Varianza Explicada por los Factores", style = "color: #2c3e50; font-weight: 500;"),
            DT::DTOutput(ns("var_table_af")),
            
            br(),
            
            h4("Interpretación", style = "color: #2c3e50; font-weight: 500;"),
            verbatimTextOutput(ns("interp_varianza_af"))
          ),
          
          #================================================
          # CARGAS FACTORIALES (LOADINGS)
          #================================================
          tabPanel(
            "Cargas Factoriales",
            value = "variables",
            
            br(),
            uiOutput(ns("alerta_loadings_af")), # Alerta local inyectada sin romper la UI
            
            wellPanel(
              p("Interpretación de cargas factoriales. Las celdas resaltadas superan la magnitud mínima de contribución.")
            ),
            
            h4(
              "Tabla de cargas",
              style = "color: #2c3e50; font-weight: 500;"
            ),
            DT::DTOutput(ns("loadings_table_af")),
            
            br(),
            
            h4(
              "Interpretación",
              style = "color: #2c3e50; font-weight: 500;"
            ),
            verbatimTextOutput(ns("interp_loadings_af")),
            
            br(),
            
            h4(
              "Comunalidades (H²) y Unicidades (U²)",
              style = "color: #2c3e50; font-weight: 500;"
            ),
            DT::DTOutput(ns("comunalidades_table")),
            
            br(),
            
            h4(
              "Interpretación de las comunalidades",
              style = "color: #2c3e50; font-weight: 500;"
            ),
            verbatimTextOutput(ns("interp_comunalidades_af"))
          ),
          
          #================================================
          # PUNTUACIONES (SCORES)
          #================================================
          tabPanel(
            "Puntuaciones Factoriales",
            value = "individuos",
            
            br(),
            uiOutput(ns("alerta_scores_af")), # Alerta local inyectada sin romper la UI
            
            h4("Puntuaciones de los individuos", style = "color: #2c3e50; font-weight: 500;"),
            plotOutput(ns("scores_plot_af"), height = "600px"),
            
            br(),
            
            h4("Tabla de puntuaciones", style = "color: #2c3e50; font-weight: 500;"),
            DT::DTOutput(ns("scores_table_af")),
            
            br(),
            
            h4("Interpretación", style = "color: #2c3e50; font-weight: 500;"),
            verbatimTextOutput(ns("interp_scores_af"))
          )
        )
      )
    )
  )
}

AF_Analisis_Server <- function(id, datos, datos_ejemplo = NULL){
  
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
    datos_preprocesados <- reactive({
      df <- if(!is.null(datos()) && nrow(datos()) > 0) datos() else datos_ejemplo
      
      crear_banner_error <- function(mensaje) {
        div(
          style = "background-color: #fdf2f2; color: #9b1c1c; border: 1px solid #fde8e8; padding: 20px; margin-bottom: 25px; border-radius: 6px;",
          tags$span(style = "font-weight: bold; font-size: 16px;", tags$i(class = "fa fa-exclamation-triangle"), " No es posible realizar el análisis."),
          br(),
          tags$span(style = "font-style: italic; color: #4b5563; font-size: 14px;", "Información: El Análisis Factorial (AF) requiere variables métricas continuas intercorrelacionadas."),
          br(), br(),
          tags$span(style = "font-weight: 500;", mensaje)
        )
      }
      
      if (is.null(df)) {
        return(list(valido = FALSE, ui_error = crear_banner_error("No se han detectado datos en el sistema.")))
      }
      
      # Filtrar únicamente columnas numéricas
      df_num <- df[, sapply(df, is.numeric), drop = FALSE]
      
      # Omitir registros incompletos (Filas con NAs)
      df_clean <- na.omit(df_num)
      
      if (ncol(df_clean) < 3) {
        return(list(valido = FALSE, ui_error = crear_banner_error("Se necesitan al menos 3 variables numéricas para justificar una reducción dimensional.")))
      }
      if (nrow(df_clean) < 15) {
        return(list(valido = FALSE, ui_error = crear_banner_error("Se requieren al menos 15 observaciones completas para validar la estructura dimensional.")))
      }
      
      return(list(valido = TRUE, base_limpia = df_clean))
    })
    
    # Renderizado único del banner superior de error
    output$mensaje_error_ui <- renderUI({
      prep <- datos_preprocesados()
      if (!prep$valido) return(prep$ui_error)
      return(NULL)
    })
    
    # Enlaces reactivos seguros garantizados por req()
    datos_num <- reactive({ req(datos_preprocesados()$valido); datos_preprocesados()$base_limpia })
    
    datos_std <- reactive({ req(datos_num()); scale(datos_num()) })
    
    #--------------------------------------------------
    # Método de los ejes principales
    #--------------------------------------------------
    af_res <- reactive({
      req(datos_num(), input$n_factores, input$metodo_rot)
      
      n_fact <- round(input$n_factores)
      met_rot <- input$metodo_rot
      
      max_factores <- floor((2 * ncol(datos_num()) + 1 - sqrt(8 * ncol(datos_num()) + 1)) / 2)
      if(n_fact > max_factores) n_fact <- max(1, max_factores)
      
      psych::fa(
        r = datos_std(), 
        nfactors = n_fact, 
        fm = "pa", 
        rotate = met_rot,
        scores = "regression"
      )
    })
    
    #--------------------------------------------------
    # SELECTORES LATERALES DINÁMICOS
    #--------------------------------------------------
    output$panel_lateral_af <- renderUI({
      req(datos_preprocesados()$valido)
      p_vars <- ncol(datos_num())
      max_factores <- max(1, floor((2 * p_vars + 1 - sqrt(8 * p_vars + 1)) / 2))
      
      elementos <- list(
        numericInput(
          ns("n_factores"),
          "Número de factores a retener:",
          value = min(2, max_factores),
          min = 1,
          max = max_factores,
          step = 1
        ),
        selectInput(
          ns("metodo_rot"),
          "Método de rotación:",
          choices = c(
            "Varimax" = "varimax", 
            "Quartimax" = "quartimax"
          ),
          selected = "varimax"
        )
      )
      
      if (!is.null(input$tabs_af) && input$tabs_af == "individuos") {
        req(input$n_factores)
        n_f <- round(input$n_factores)
        elementos <- c(elementos, list(
          hr(),
          h5("Gráfico de Factores"),
          selectInput(ns("af_eje_x"), "Factor Eje X:", choices = 1:n_f, selected = 1),
          selectInput(ns("af_eje_y"), "Factor Eje Y:", choices = 1:n_f, selected = min(2, n_f))
        ))
      }
      
      if (!is.null(input$tabs_af) && input$tabs_af == "variables") {
        elementos <- c(elementos, list(
          hr(),
          h5("Filtrado de Cargas (Loadings)"),
          selectInput(
            ns("tipo_cargas"),
            "Mostrar matriz:",
            choices = c(
              "Cargas rotadas" = "rotadas",
              "Cargas sin rotar" = "sin_rotar"
            ),
            selected = "rotadas"
          ),
          sliderInput(
            ns("af_magnitud_carga"),
            "Magnitud mínima de contribución:",
            min = 0, max = 1, value = 0.3, step = 0.05
          )
        ))
      }
      
      tagList(elementos)
    })
    
    #--------------------------------------------------
    # BOTÓN DE DESCARGA DINÁMICO
    #--------------------------------------------------
    output$ui_dl_af <- renderUI({
      req(datos_preprocesados()$valido, input$n_factores)
      f <- round(input$n_factores)
      downloadButton(
        ns("dl_af"),
        paste0("Descargar los ", f, " factores seleccionados"),
        class = "btn-success",
        style = "width: 100%;"
      )
    })
    
    #--------------------------------------------------
    # TABLAS DE DATOS 
    #--------------------------------------------------
    output$dataset_af <- DT::renderDT({
      df_base <- if(!is.null(datos()) && nrow(datos()) > 0) datos() else datos_ejemplo
      req(df_base)
      
      DT::datatable(
        df_base[complete.cases(df_base[, sapply(df_base, is.numeric)]), ], 
        options = list(paging = FALSE, scrollY = "400px", scrollX = TRUE, autoWidth = TRUE),
        class = 'cell-border stripe hover compact'
      ) %>% 
        DT::formatRound(columns = which(sapply(df_base, is.numeric)), digits = 3)
    })
    
    #--------------------------------------------------
    # DIAGNÓSTICO Y DEMÁS RENDERERS REQUERIDOS (Sin alteraciones estructurales)
    #--------------------------------------------------
    output$dataset_std_af <- DT::renderDT({
      req(datos_std())
      DT::datatable(
        round(as.data.frame(datos_std()), 3), 
        options = list(paging = FALSE, scrollY = "400px", scrollX = TRUE, autoWidth = TRUE),
        class = 'cell-border stripe hover compact'
      )
    })
    
    output$bartlett_test_af <- renderPrint({
      req(datos_std())
      psych::cortest.bartlett(cor(datos_std()), n = nrow(datos_std()))
    })
    
    output$alerta_diag_af <- renderUI({
      prep <- datos_preprocesados()
      if (!prep$valido) return(prep$ui_error)
      return(NULL)
    })
    
    output$alerta_datos_af <- renderUI({
      prep <- datos_preprocesados()
      if (!prep$valido) return(prep$ui_error)
      return(NULL)
    })
    
    output$alerta_scree_af <- renderUI({
      prep <- datos_preprocesados()
      if (!prep$valido) return(prep$ui_error)
      return(NULL)
    })
    
    output$alerta_loadings_af <- renderUI({
      prep <- datos_preprocesados()
      if (!prep$valido) return(prep$ui_error)
      return(NULL)
    })
    
    output$alerta_scores_af <- renderUI({
      prep <- datos_preprocesados()
      if (!prep$valido) return(prep$ui_error)
      return(NULL)
    })
    
    output$kmo_test <- renderPrint({
      req(datos_num())
      psych::KMO(datos_num())
    })
    
    output$corrplot_af <- renderPlot({
      req(datos_std())
      corrplot::corrplot(
        cor(datos_std()),
        method = "color",
        type = "upper",
        tl.col = "black"
      )
    })
    
    output$scree_af <- renderPlot({
      req(datos_num(), input$n_factores)
      f_sel <- round(input$n_factores)
      ev <- eigen(cor(datos_num()))$values
      plot(
        ev, 
        type = "b", 
        xaxt = "n", 
        xlab = "Factores", 
        ylab = "Eigenvalues (Valores Propios)",
        main = "Scree Plot (Análisis Factorial)"
      )
      axis(1, at = 1:length(ev), labels = 1:length(ev))
      abline(h = 1, col = "blue", lty = 3) 
      abline(v = f_sel, col = "red", lty = 2)
      mtext(
        sprintf("Corte seleccionado en %d factores", f_sel), 
        side = 3, 
        col = "red"
      )
    })
    
    output$var_table_af <- DT::renderDT({
      req(af_res())
      var_info <- af_res()$Vaccounted
      req(var_info)
      var_transpuesta <- as.data.frame(t(var_info))
      DT::datatable(
        round(var_transpuesta, 3),
        options = list(paging = FALSE, searching = FALSE, info = FALSE, scrollX = TRUE, autoWidth = TRUE),
        class = 'cell-border stripe hover compact'
      )
    })
    
    output$interp_varianza_af <- renderText({
      req(af_res(), input$n_factores)
      f <- as.integer(input$n_factores)
      var_info <- af_res()$Vaccounted
      req(var_info, "Cumulative Var" %in% rownames(var_info))
      f_valido <- min(f, ncol(var_info))
      var_acumulada_f <- round(var_info["Cumulative Var", f_valido] * 100, 2)
      paste0(
        "El modelo actual con ", f_valido, " factores logra explicar un total acumulado de ", var_acumulada_f, "% de la varianza total de los datos originales.\n\n",
        "Ejemplo práctico (Dataset 'wines'): Al extraer factores con rotación Varimax sobre wines, se suelen identificar 3 ",
        "dimensiones subyacentes bien definidas."
      )
    })
    
    output$loadings_table_af <- DT::renderDT({
      req(af_res(), input$tipo_cargas)
      umbral <- if(!is.null(input$af_magnitud_carga)) input$af_magnitud_carga else 0.3
      if(input$tipo_cargas == "rotadas"){
        cargas <- as.data.frame(unclass(af_res()$loadings))
      } else {
        af_sin_rotar <- psych::fa(r = datos_std(), nfactors = round(input$n_factores), fm = "pa", rotate = "none")
        cargas <- as.data.frame(unclass(af_sin_rotar$loadings))
      }
      DT::datatable(
        round(cargas, 3),
        options = list(paging = FALSE, scrollY = "400px", scrollX = TRUE, autoWidth = TRUE),
        class = 'cell-border stripe hover compact'
      ) %>%
        DT::formatStyle(
          columns = 1:ncol(cargas),
          backgroundColor = DT::styleInterval(cuts = c(-umbral, umbral), values = c("#fcd7d7", "transparent", "#d4edda")),
          fontWeight = DT::styleInterval(cuts = c(-umbral, umbral), values = c("bold", "normal", "bold"))
        )
    })
    
    output$interp_loadings_af <- renderText({
      req(input$n_factores)
      umbral <- if(!is.null(input$af_magnitud_carga)) input$af_magnitud_carga else 0.3
      paste0(
        "Se evalúan las cargas estructurales de la matriz factorial de variables.\n",
        "Se han resaltado en VERDE las cargas positivas significativas y en ROJO las negativas significativas (magnitud absoluta > ", umbral, ").\n\n",
        "Ejemplo práctico (Dataset 'wines'): Tras aplicar una rotación Varimax, verás que el 'Factor 1' recolecta cargas ",
        "muy altas y unificadas en Phenols y Flavanoids (resaltados), lo que permite etiquetarlo claramente como la dimensión de 'Concentración Polifenólica'. ",
        "El 'Factor 2' se asocia fuertemente con Alcohol e Intensidad de Color, reflejando el 'Cuerpo y Grado' del vino."
      )
    })
    
    output$comunalidades_table <- DT::renderDT({
      req(af_res())
      af <- af_res()
      df_comun <- data.frame(
        Variable = names(af$communality),
        "Comunalidades (H²)" = round(af$communality, 3),
        "Unicidades (U²)" = round(af$uniquenesses, 3),
        check.names = FALSE
      )
      DT::datatable(
        df_comun, rownames = FALSE, 
        options = list(paging = FALSE, scrollY = "300px", scrollX = TRUE, autoWidth = TRUE),
        class = "cell-border stripe hover compact"
      )
    })
    
    output$interp_comunalidades_af <- renderText({
      req(af_res())
      h2 <- af_res()$communality
      buenas <- sum(h2 >= 0.50); medias <- sum(h2 >= 0.30 & h2 < 0.50); bajas <- sum(h2 < 0.30)
      paste0(
        "Las comunalidades (H²) indican la proporción de la variabilidad de cada variable explicada por los factores retenidos, mientras que la unicidad (U²) representa la parte no explicada.\n\n",
        "En este análisis se obtienen:\n",
        "- Variables bien representadas (H² ≥ 0.50): ", buenas, ".\n",
        "- Variables con representation moderada (0.30 ≤ H² < 0.50): ", medias, ".\n",
        "- Variables poco representadas (H² < 0.30): ", bajas, ".\n\n",
        "En general, las variables con comunalidades altas pueden interpretarse con mayor confianza dentro del modelo factorial, mientras que las de comunalidad baja aportan poca información común."
      )
    })
    
    output$scores_plot_af <- renderPlot({
      req(af_res())
      scores_df <- as.data.frame(af_res()$scores)
      e_x <- if(!is.null(input$af_eje_x)) as.numeric(input$af_eje_x) else 1
      e_y <- if(!is.null(input$af_eje_y)) as.numeric(input$af_eje_y) else min(2, ncol(scores_df))
      req(ncol(scores_df) >= max(e_x, e_y))
      plot(
        scores_df[, e_x], scores_df[, e_y],
        xlab = paste("Factor", e_x), ylab = paste("Factor", e_y),
        main = "Espacio Factorial de los Individuos (Scores)",
        col = "#1a446c", pch = 19, cex = 1.2
      )
      grid()
      abline(h = 0, v = 0, lty = 2, col = "gray")
    })
    
    output$scores_table_af <- DT::renderDT({
      req(af_res())
      DT::datatable(
        round(as.data.frame(af_res()$scores), 3),
        options = list(paging = FALSE, scrollY = "400px", scrollX = TRUE, autoWidth = TRUE),
        class = 'cell-border stripe hover compact'
      )
    })
    
    output$interp_scores_af <- renderText({
      req(input$n_factores)
      paste0(
        "Muestra las coordenadas estimadas de cada una de las observaciones respecto a las nuevas variables latentes (factores).\n\n",
        "Ejemplo práctico (Dataset 'wines'): En este plano factorial de observaciones, las muestras químicas ",
        "se agruparán en función de su perfil subyacente. Los vinos con valores negativos altos en el factor polifenólico ",
        "se concentrarán juntos, permitiendo discriminar el origen o variedad sin necesidad de etiquetado previo."
      )
    })
    
    output$dl_af <- downloadHandler(
      filename = function() { "AF_Resultados_Factores.csv" },
      content = function(file) {
        req(af_res())
        write.csv(as.data.frame(af_res()$scores), file, row.names = TRUE)
      }
    )
  }) 
}
# =====================================================
#   AUTOEVALUACIÓN 
# =====================================================  
AF_Auto_UI <- function(id) {
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
        title = "➕ Gestión: Añadir pregunta personalizada de AF",  
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
        
        actionButton(ns("add"), "Guardar pregunta en el banco del AF", class = "btn-success btn-sm mt-2")
      )
    )
  )
}

AF_Auto_Server <- function(id) {
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
        texto = "¿Qué representa la matriz A?",
        opciones = c(
          "Matriz de covarianzas", 
          "Matriz de cargas factoriales", 
          "Matriz de datos originales", 
          "Matriz identidad"
        ),
        correcta = "Matriz de cargas factoriales"
      ),
      list(
        texto = "¿Cuál es el objetivo principal del análisis factorial?",
        opciones = c(
          "Clasificar observaciones", 
          "Calcular medias", 
          "Reducir dimensionalidad explicando la covarianza entre variables", 
          "Eliminar variables"
        ),
        correcta = "Reducir dimensionalidad explicando la covarianza entre variables"
      ),
      list(
        texto = "¿Qué son los factores comunes?",
        opciones = c(
          "Factores no observados que explican la correlación", 
          "Variables observadas", 
          "Errores aleatorios", 
          "Variables dependientes"
        ),
        correcta = "Factores no observados que explican la correlación"
      ),
      list(
        texto = "¿Por qué se rota la matriz factorial?",
        opciones = c(
          "Para reducir variables", 
          "Para calcular medias", 
          "Para eliminar factores", 
          "Para aumentar la interpretabilidad"
        ),
        correcta = "Para aumentar la interpretabilidad"
      ),
      list(
        texto = "¿Qué significa la comunalidad de una variable?",
        opciones = c(
          "Proporción de varianza explicada por los factores", 
          "Varianza total de la variable", 
          "Media de la variable", 
          "Error de medición"
        ),
        correcta = "Proporción de varianza explicada por los factores"
      ),
      list(
        texto = "¿Qué indica una carga factorial alta?",
        opciones = c(
          "Baja relación con el factor", 
          "Error alto", 
          "Alta relación entre variable y factor", 
          "Independencia"
        ),
        correcta = "Alta relación entre variable y factor"
      ),
      list(
        texto = "¿Qué criterio se usa para decidir el número de factores?",
        opciones = c(
          "Media de variables", 
          "Tamaño de la muestra", 
          "Regla de Kaiser (eigenvalores > 1)", 
          "Varianza mínima"
        ),
        correcta = "Regla de Kaiser (eigenvalores > 1)"
      ),
      list(
        texto = "¿Qué mide el índice KMO?",
        opciones = c(
          "Número de factores", 
          "Adecuación muestral para análisis factorial", 
          "Distancia entre individuos", 
          "Sesgo de rotación"
        ),
        correcta = "Adecuación muestral para análisis factorial"
      ),
      list(
        texto = "¿Qué evalúa la prueba de Bartlett?",
        opciones = c(
          "Normalidad", 
          "Varianza total", 
          "Independencia de factores", 
          "Si la matriz de correlación es identidad"
        ),
        correcta = "Si la matriz de correlación es identidad"
      ),
      list(
        texto = "¿Qué tipo de rotación usar si factores están correlacionados?",
        opciones = c(
          "Oblicua", 
          "Ortogonal", 
          "Ninguna", 
          "PCA"
        ),
        correcta = "Oblicua"
      ),
      list(
        texto = "¿Qué compone la varianza total?",
        opciones = c(
          "Solo comunalidad", 
          "Comunalidad + varianza única", 
          "Solo error", 
          "Solo factores"
        ),
        correcta = "Comunalidad + varianza única"
      ),
      list(
        texto = "¿Qué asumen los errores en AF?",
        opciones = c(
          "Están correlacionados", 
          "Son factores comunes", 
          "No están correlacionados entre sí ni con factores", 
          "Son variables observadas"
        ),
        correcta = "No están correlacionados entre sí ni con factores"
      ),
      list(
        texto = "Si la variable 'Magnesium' presenta una comunalidad de 0.22 tras la extracción factorial en los datos de 'wines', ¿cómo debe interpretarse analíticamente este resultado?",
        opciones = c(
          "Los factores comunes solo logran explicar un 22% de su varianza, quedando el 78% como varianza única o error", 
          "El magnesio es la variable predictora más importante del modelo", 
          "La variable debe eliminarse porque tiene valores negativos", 
          "El factor extraído es idéntico a la variable magnesio"
        ),
        correcta = "Los factores comunes solo logran explicar un 22% de su varianza, quedando el 78% como varianza única o error"
      ),
      list(
        texto = "Si realizas una rotación 'Varimax' y observas que 'Alcohol' y 'Color Intensity' se desplazan casi por completo al Factor 2, liberando de carga al Factor 1, ¿qué objetivo estadístico has alcanzado?",
        opciones = c(
          "Aumentar artificialmente el número de variables originales", 
          "Estructura simple: facilitar la separación e interpretación de los factores", 
          "Eliminar los errores aleatorios de las barricas", 
          "Cambiar los coeficientes de correlación original"
        ),
        correcta = "Estructura simple: facilitar la separación e interpretación de los factores"
      ),
      list(
        texto = "¿Cúantos factores deberías retener en el dataset wines",
        opciones = c(
          "Ninguno,se descartar todo el análisis por baja varianza", 
          "Hay que retener los 13 factores obligatoriamente", 
          "Los 3 primeros", 
          "Se utilizar únicamente el primer factor"
        ),
        correcta = "Los 3 primeros"
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

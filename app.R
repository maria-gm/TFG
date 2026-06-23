# =====================================================
#  MÓDULOS (CONSERVANDO TODOS TUS SOURCES)
# =====================================================
source("global.R")
source("Menu.R")
source("modules/reduccion/reduccion_selector.R")
source("modules/reduccion/pca_module.R")
source("modules/reduccion/af_module.R")
source("modules/reduccion/biplot_module.R")
source("modules/agrupamiento/clusteres_jerarquicos_module.R")
source("modules/agrupamiento/k_means_module.R")
source("modules/agrupamiento/dbscan_module.R")
source("modules/agrupamiento/agrupamiento_selector_module.R")
source("modules/regresion multivariante/regresion_selector.R")
source("modules/regresion multivariante/regresion_multiple.R")
source("modules/regresion multivariante/regularizacion.R")
source("modules/clasificacion/clasificacion_selector.R")
source("modules/clasificacion/regresion_logistica.R")
source("modules/clasificacion/lda.R")
source("modules/clasificacion/arboles.R")


# =====================================================
# INTERFAZ DE USUARIO (UI) GENERAL
# =====================================================
ui <- page_sidebar(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  
  tags$head(
    tags$style(HTML("
      body { background-color: #f8fafc; }
      .sidebar { background-color: #1e1e2f !important; color: #ffffff !important; border-right: none !important; }
      .sidebar .nav-link { color: #a0aec0 !important; border-radius: 4px; margin-bottom: 4px; display: block; text-align: left; width: 100%; background: none; border: none; padding: 8px 16px; }
      .sidebar .nav-link.active { background-color: #31314d !important; color: #ffffff !important; font-weight: bold; }
      .sidebar .nav-link:hover { background-color: #2d2d3f !important; color: #ffffff !important; }
      .sidebar-title { color: #ffffff !important; font-size: 1.2rem; font-weight: bold; padding-bottom: 10px; border-bottom: 1px solid #31314d; margin-bottom: 15px; }
    "))
  ),
  
  # CONFIGURACIÓN DEL MENÚ LATERAL (SOLO BOTONES)
  sidebar = sidebar(
    id = "menu_lateral_principal",
    title = div(class = "sidebar-title", "Índice"),
    width = 300,
    open = "open",
    
    # Enlaces limpios sin meter las UI de los módulos aquí dentro
    actionLink("go_menu", "Inicio", class = "nav-link active"),
    actionLink("go_reduccion", "Reducción de Dimensión", class = "nav-link"),
    actionLink("go_agrupamiento", "Agrupamiento", class = "nav-link"),
    actionLink("go_regresion", "Regresión", class = "nav-link"),
    actionLink("go_clasificacion", "Clasificación", class = "nav-link")
  ),
  
  # CUERPO PRINCIPAL (DERECHA)
  tags$div(
    style = "display: flex; flex-direction: column; min-height: 85vh;",
    
    # 1. Banner Superior
    tags$div(
      style = "padding: 24px; background: linear-gradient(135deg, #1a446c 0%, #2a69ac 100%); border-radius: 8px; color: white; margin-bottom: 25px; box-shadow: 0 4px 6px -1px rgb(0 0 0 / 0.1);",
      h2("Herramienta de Análisis Multivariante", style = "font-weight: 700; margin-bottom: 8px; letter-spacing: -0.5px;"),
      p("Explora, analiza y visualiza datos mediante técnicas estadísticas avanzadas", style = "opacity: 0.9; font-size: 1.05rem; margin: 0;")
    ),
    
    # 2. Contenedor de las pestañas reales (Se renderizan una sola vez aquí en el centro)
    navset_hidden(
      id = "tabs_navegacion",
      nav_panel_hidden("menu_tab", Menu_UI("menu_inicio_mod")),
      nav_panel_hidden("reduccion_tab", Reduccion_UI("reduccion_mod")),
      nav_panel_hidden("agrupamiento_tab", Agrupamiento_UI("agrupamiento_mod")),
      nav_panel_hidden("regresion_tab", Regresion_UI("regresion_mod")),
      nav_panel_hidden("clasificacion_tab", Clasificacion_UI("clasificacion_mod"))
    ),
    
    # 3. Pie de página institucional (Footer)
    tags$footer(
      style = "margin-top: auto; padding: 20px 0; background-color: #ffffff; border-top: 1px solid #e2e8f0; width: 100%; border-radius: 4px;",
      fluidRow(
        style = "align-items: center; display: flex;",
        column(
          width = 4, 
          style = "display: flex; justify-content: center; align-items: center;",
          tags$img(src = "logo.png", height = "60px", alt = "Logo Institucional", style = "max-width: 100%;")
        ),
        column(
          width = 8, 
          style = "color: #4a5568; font-size: 0.9rem; line-height: 1.6; text-align: left; padding-left: 30px; border-left: 2px solid #e2e8f0;",
          tags$div(
            strong("Trabajo de Fin de Grado (TFG)"), " - Universidad de Salamanca", br(),
            span("Autor: ", style = "color: #718096;"), strong("María Gordo Martín"), br(),
            span("Titulación: ", style = "color: #718096;"), "Grado en Estadística", br(),
            span("Curso Académico: ", style = "color: #718096;"), "2025/2026"
          )
        )
      )
    )
  )
)

# =====================================================
# SERVIDOR (SERVER) GENERAL
# =====================================================
server <- function(input, output, session) {
  
  # Gestión de navegación nativa limpia mediante nav_select
  observeEvent(input$go_menu, {
    nav_select("tabs_navegacion", "menu_tab")
    shinyjs::runjs("$('.sidebar .nav-link').removeClass('active'); $('#go_menu').addClass('active');")
  })
  
  observeEvent(input$go_reduccion, {
    nav_select("tabs_navegacion", "reduccion_tab")
    shinyjs::runjs("$('.sidebar .nav-link').removeClass('active'); $('#go_reduccion').addClass('active');")
    
    # NUEVA LÍNEA: Fuerza al selector interno a regresar a la portada común con gráficos
    session$userData$reset_reduccion <- runif(1) 
  })
  
  observeEvent(input$go_agrupamiento, {
    nav_select("tabs_navegacion", "agrupamiento_tab")
    shinyjs::runjs("$('.sidebar .nav-link').removeClass('active'); $('#go_agrupamiento').addClass('active');")
    
    # NUEVA LÍNEA: Reinicia el módulo de clustering a su portada común limpia
    session$userData$reset_agrupamiento <- runif(1) 
  })
  observeEvent(input$go_regresion, {
    nav_select("tabs_navegacion", "regresion_tab")
    shinyjs::runjs("$('.sidebar .nav-link').removeClass('active'); $('#go_regresion').addClass('active');")
    
    # NUEVA LÍNEA: Resetea el módulo de regresión multivariante a su landing page de gráficos
    session$userData$reset_regresion <- runif(1) 
  })
  
  observeEvent(input$go_regresion, {
    nav_select("tabs_navegacion", "regresion_tab")
    shinyjs::runjs("$('.sidebar .nav-link').removeClass('active'); $('#go_regresion').addClass('active');")
    session$userData$reset_regresion <- runif(1) 
  })
  
  observeEvent(input$go_clasificacion, {
    nav_select("tabs_navegacion", "clasificacion_tab")
    shinyjs::runjs("$('.sidebar .nav-link').removeClass('active'); $('#go_clasificacion').addClass('active');")
    session$userData$reset_clasificacion <- runif(1) 
  })
  
  # Objeto reactivo para la carga de archivos compartida
  datos_usuario <- reactive({ NULL })
  
  # Inicialización de servidores modulares (se quedan exactamente igual)
  Menu_Server("menu_inicio_mod")
  Reduccion_Server("reduccion_mod", datos = datos_usuario, datos_ejemplo = datos_ejemplo)
  Agrupamiento_Server("agrupamiento_mod", datos = datos_usuario, datos_ejemplo = datos_ejemplo)
  Regresion_Server("regresion_mod", datos = datos_usuario, datos_ejemplo = datos_ejemplo)
  Clasificacion_Server("clasificacion_mod", datos = datos_usuario, datos_ejemplo = datos_ejemplo)
}

shinyApp(ui = ui, server = server)

  
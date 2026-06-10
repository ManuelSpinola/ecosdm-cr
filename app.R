# ============================================================
# OVS-CR · Explorador de Especies Terrestres · Costa Rica
# EcoSuite · Shiny app modular · bslib + leaflet + leafgl + h3sdm
# Manuel Spínola · ICOMVIS · UNA
# ============================================================

library(shiny)
library(bslib)
library(bsicons)
library(shinyjs)
library(leaflet)
library(leafgl)

# Configuración compartida (colores, tema, escalas)
source("R/helpers.R")

# Módulos
source("R/utils_data.R")
source("R/mod_sidebar.R")
source("R/mod_registros.R")
source("R/mod_modelo.R")
source("R/mod_mapas.R")
source("R/mod_metricas.R")

# ── UI ───────────────────────────────────────────────────────
ui <- page_navbar(
  title = div(
    tags$img(src = "logo_ICOMVIS_circular.png",
             height = "34px", class = "me-2",
             style = "border-radius:50%; vertical-align:middle;"),
    span("OVS · Distribución de Especies · Costa Rica",
         style = "vertical-align:middle;")
  ),
  theme    = tema_app,
  fillable = TRUE,
  header   = useShinyjs(),
  footer   = div(
    style = paste0(
      "background-color:#a31e32; color:#ffffff; ",
      "text-align:center; padding:6px 12px; ",
      "font-size:0.75rem; line-height:1.6;"
    ),
    "Manuel Spínola · ICOMVIS · Universidad Nacional · Costa Rica"
  ),

  # ── Panel principal ──────────────────────────────────────
  nav_panel(
    title = tagList(bs_icon("globe-americas", class = "me-1"),
                    "Explorador"),

    layout_sidebar(
      fillable = TRUE,

      sidebar = sidebar(
        width = 300,
        bg    = colores$fondo,
        mod_sidebar_ui("sidebar")
      ),

      navset_card_tab(
        id = "tabs_resultados",

        nav_panel(
          title = tagList(bs_icon("pin-map", class = "me-1"),
                          "Registros"),
          mod_registros_ui("registros")
        ),

        nav_panel(
          title = tagList(bs_icon("map-fill", class = "me-1"),
                          "Dist. actual"),
          mod_mapas_ui("mapas_presente", tipo = "presente")
        ),

        nav_panel(
          title = tagList(bs_icon("thermometer-half", class = "me-1"),
                          "Dist. futura"),
          mod_mapas_ui("mapas_futuro", tipo = "futuro")
        ),

        nav_panel(
          title = tagList(bs_icon("shield-check", class = "me-1"),
                          "Confiabilidad"),
          mod_mapas_ui("mapas_aoa", tipo = "aoa")
        ),

        nav_panel(
          title = tagList(bs_icon("shield-shaded", class = "me-1"),
                          "Conf. futura"),
          mod_mapas_ui("mapas_aoa_futuro", tipo = "aoa_futuro")
        ),

        nav_panel(
          title = tagList(bs_icon("bar-chart", class = "me-1"),
                          "Métricas"),
          mod_metricas_ui("metricas")
        )
      )
    )
  ),

  # ── Acerca de ────────────────────────────────────────────
  nav_panel(
    title = tagList(bs_icon("info-circle", class = "me-1"),
                    "Acerca de"),
    div(
      class = "p-4",
      style = "max-width:800px; margin:auto; min-height:calc(100vh - 120px); display:flex; flex-direction:column;",

      h4("Explorador de Especies Terrestres de Costa Rica",
         style = paste0("color:", colores$primario, "; font-weight:700;")),
      p(class = "text-muted",
        "Versión 2.0 · ICOMVIS-UNA · ",
        tags$a(href = "https://icomvis.una.ac.cr",
               target = "_blank", "icomvis.una.ac.cr")),
      tags$hr(),

      layout_columns(
        col_widths = c(6, 6), fill = FALSE,

        card(
          card_header(bs_icon("hexagon-fill", class = "me-1"),
                      "Motor: h3sdm"),
          card_body(
            p("Modelado de distribuci\u00f3n de especies basado en grillas
               hexagonales H3. Las covariables CHELSA est\u00e1n pre-procesadas
               por hex\u00e1gono para resoluciones 6, 7 y 8."),
            p("Datos de ocurrencia: ",
              strong("GBIF, iNaturalist, BiodataCR"), "."),
            p("Modelo: ", strong("GAM espacial"),
              " (Modelo Aditivo Generalizado con estructura espacial)."),
            p("Los \u00edndices espaciales H3 permiten explorar patrones a
               m\u00faltiples resoluciones \u2014 de paisaje a microh\u00e1bitat \u2014 con
               celdas de \u00e1rea uniforme."),
            p("Proyecci\u00f3n futura bajo escenario de cambio clim\u00e1tico ",
              strong("SSP5-8.5"), " usando variables CHELSA.")
          )
        ),

        card(
          card_header(bs_icon("funnel", class = "me-1"),
                      "Filtro de outliers ambientales"),
          card_body(
            p("Antes del modelado, los registros de presencia se filtran
               autom\u00e1ticamente usando la ",
              strong("distancia de Mahalanobis"), " en espacio ambiental."),
            p("Los registros con D^2 superior al percentil 97.5 de la
               distribuci\u00f3n chi-cuadrado se excluyen del entrenamiento,
               eliminando observaciones ecol\u00f3gicamente incoherentes
               (p.ej. especies de altura con registros en tierras bajas)."),
            p("Las pseudoausencias no son afectadas por este filtro.")
          )
        )
      ),

      card(
        card_header(bs_icon("shield-check", class = "me-1"),
                    "\u00c1rea de Aplicabilidad (AOA)"),
        card_body(
          p("El AOA delimita la regi\u00f3n donde el modelo puede predecir
             con confianza, bas\u00e1ndose en la similitud ambiental con los
             datos de entrenamiento."),
          p("Se muestran 4 mapas: AOA binario, \u00cdndice de Disimilaridad,
             e idoneidad continua y categ\u00f3rica dentro del AOA.")
        )
      ),

      div(
        style = "margin-top:auto; padding-top:1rem;",
        card(
          card_header(bs_icon("person", class = "me-1"), "Cr\u00e9ditos"),
          card_body(
            p(strong("Manuel Sp\u00ednola"),
              " \u2014 ICOMVIS, Universidad Nacional de Costa Rica"),
            p("App desarrollada con asistencia de ",
              tags$a("Claude (Anthropic)",
                     href = "https://www.anthropic.com",
                     target = "_blank"), " y con ",
              tags$a("h3sdm",
                     href = "https://github.com/ManuelSpinola/h3sdm",
                     target = "_blank"), ", ",
              tags$a("Shiny",
                     href = "https://shiny.posit.co",
                     target = "_blank"), ", ",
              tags$a("bslib",
                     href = "https://rstudio.github.io/bslib",
                     target = "_blank"), " y ",
              tags$a("leafgl",
                     href = "https://github.com/r-spatial/leafgl",
                     target = "_blank"), ".")
          )
        )
      )
    )
  ),

  nav_spacer(),
  nav_item(
    tags$a(
      href = "https://www.icomvis.una.ac.cr/", target = "_blank",
      class = "nav-link",
      bs_icon("house"), " ICOMVIS"
    )
  )
)

# ── Server ───────────────────────────────────────────────────
server <- function(input, output, session) {

  estado <- reactiveValues(
    registros_sf       = NULL,
    modelo_ajustado    = NULL,
    prediccion_sf      = NULL,
    pred_futuro_sf     = NULL,
    aoa_sf             = NULL,
    aoa_futuro_sf      = NULL,
    dat_rv             = NULL,
    cv_split_rv        = NULL,
    algoritmo          = "rf",
    resolucion         = "7",
    error_sin_registros = FALSE
  )

  sidebar_vals <- mod_sidebar_server("sidebar", estado)
  mod_registros_server("registros", estado, sidebar_vals)
  mod_modelo_server("modelo_interno", estado, sidebar_vals)
  mod_mapas_server("mapas_presente", estado, tipo = "presente")
  mod_mapas_server("mapas_futuro",   estado, tipo = "futuro")
  mod_mapas_server("mapas_aoa",        estado, tipo = "aoa")
  mod_mapas_server("mapas_aoa_futuro", estado, tipo = "aoa_futuro")
  mod_metricas_server("metricas", estado)
}

shinyApp(ui, server)

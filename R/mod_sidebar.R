# ============================================================
# mod_sidebar.R
# Explorador de Especies Terrestres · Costa Rica
# UI pública: solo especie + resolución + botón
# Parámetros técnicos fijos internamente
# OVS · ICOMVIS · UNA
# ============================================================

# ── Valores técnicos fijos (no expuestos al usuario) ─────────
.LIMITE      <- 2000L   # máx registros por fuente
.N_PSEUDOABS <- 500L    # pseudoausencias
.BUFFER_K    <- 1L      # anillos H3 de exclusión
.ALGORITMO   <- "gam"    # Random Forest
.FUENTES     <- c("gbif", "inat", "biodatacr")

# Etiquetas públicas para las resoluciones
.res_choices <- c(
  "6 — Regional (~36 km²)"   = "6",
  "7 — Local (~5 km²)"       = "7",
  "8 — Detallado (~0.7 km²)" = "8"
)

# ── UI ────────────────────────────────────────────────────────
mod_sidebar_ui <- function(id) {
  ns <- NS(id)
  tagList(

    # Logo + identidad
    div(
      class = "text-center mb-3",
      tags$img(
        src   = "logo_ICOMVIS_circular.png",
        style = "width:65%; max-width:160px; height:auto;"
      ),
      p(class = "small text-muted mt-1 mb-0", "ICOMVIS · UNA")
    ),

    tags$hr(class = "my-2"),

    # Título de la app
    div(
      class = "mb-3 text-center",
      p(class = "fw-bold mb-0",
        style = "color:#a31e32; font-size:0.95rem; line-height:1.3;",
        "Explorador de Especies",
        tags$br(),
        "Terrestres de Costa Rica")
    ),

    tags$hr(class = "my-2"),

    # ── Input 1: Especie ─────────────────────────────────────
    div(
      class = "mb-3",
      tags$label(
        class = "form-label fw-bold",
        style = "font-size:0.9rem;",
        bs_icon("search", class = "me-1"),
        "¿Qué especie querés explorar?"
      ),
      textInput(
        ns("especie"),
        label       = NULL,
        placeholder = "Ej. Panthera onca"
      ),
      p(class = "small text-muted mb-0",
        bs_icon("info-circle", class = "me-1"),
        "Usá el nombre científico para mejores resultados.")
    ),

    # ── Input 2: Resolución ──────────────────────────────────
    div(
      class = "mb-3",
      tags$label(
        class = "form-label fw-bold",
        style = "font-size:0.9rem;",
        bs_icon("hexagon-fill", class = "me-1"),
        "Tamaño de los hexágonos"
      ),
      selectInput(
        ns("resolucion"),
        label    = NULL,
        choices  = .res_choices,
        selected = "7"
      ),
      uiOutput(ns("info_res"))
    ),

    tags$hr(class = "my-2"),

    # ── Botón principal ──────────────────────────────────────
    actionButton(
      ns("btn_modelar"),
      tagList(bs_icon("play-fill", class = "me-1"),
              "Ver distribución"),
      class = "btn-primary w-100",
      style = "font-weight:600; font-size:1rem; padding:0.5rem;"
    ),

    br(), br(),

    # Spinner de descarga (oculto por defecto)
    shinyjs::hidden(
      div(
        id = ns("spinner_descarga"),
        class = "alert alert-info small py-2 px-3 mb-2 text-center",
        tags$span(
          class = "spinner-border spinner-border-sm me-2",
          role  = "status"
        ),
        "Descargando registros…"
      )
    ),

    # Estado / feedback
    uiOutput(ns("estado_modelo")),

    tags$hr(class = "my-2"),

    # ── Guía de pestañas ────────────────────────────────────
    div(
      class = "small text-muted",
      p(class = "fw-bold mb-2", style = "font-size:0.8rem;",
        "Resultados:"),
      p(class = "mb-1",
        bs_icon("pin-map", class = "me-1"),
        tags$b("Registros"),
        " — dónde ha sido observada"),
      p(class = "mb-1",
        bs_icon("map-fill", class = "me-1"),
        tags$b("Distribución actual"),
        " — dónde puede vivir hoy"),
      p(class = "mb-1",
        bs_icon("thermometer-half", class = "me-1"),
        tags$b("Distribución futura"),
        " — proyección climática 2061–2080"),
      p(class = "mb-0",
        bs_icon("shield-check", class = "me-1"),
        tags$b("Confiabilidad"),
        " — zonas de predicción segura")
    ),

    tags$hr(class = "my-2"),

    # Fuentes de datos
    p(class = "small text-muted text-center mb-1",
      bs_icon("database", class = "me-1"),
      tags$a("GBIF", href = "https://www.gbif.org", target = "_blank",
             style = "color:inherit;"),
      " · ",
      tags$a("iNaturalist", href = "https://www.inaturalist.org",
             target = "_blank", style = "color:inherit;"),
      " · ",
      tags$a("BiodataCR", href = "https://biodatacr.ac.cr",
             target = "_blank", style = "color:inherit;")
    ),

    p(class = "small text-muted text-center mb-0",
      "\U0001f1e8\U0001f1f7 Costa Rica"),

  )
}

# ── Server ────────────────────────────────────────────────────
mod_sidebar_server <- function(id, estado) {
  moduleServer(id, function(input, output, session) {

    # Info de resolución en lenguaje público
    output$info_res <- renderUI({
      desc <- switch(input$resolucion,
        "6" = "Útil para ver patrones amplios a escala de paisaje.",
        "7" = "Balance ideal entre detalle y velocidad. Recomendado.",
        "8" = "Mayor detalle — puede tardar más en especies con muchos registros."
      )
      div(
        class = "alert alert-light small py-2 px-3 mb-0 mt-1",
        bs_icon("info-circle", class = "me-1"),
        desc
      )
    })

    # Feedback de estado al usuario
    output$estado_modelo <- renderUI({
      if (!is.null(estado$prediccion_sf)) {
        div(
          class = "alert alert-success small py-2 px-3 mb-0",
          bs_icon("check-circle-fill", class = "me-1"),
          "¡Listo! Explorá los resultados en las pestañas."
        )
      } else if (!is.null(estado$registros_sf)) {
        div(
          class = "alert alert-info small py-2 px-3 mb-0",
          bs_icon("hourglass-split", class = "me-1"),
          "Descargando registros y modelando…"
        )
      } else if (isTRUE(estado$error_sin_registros)) {
        div(
          class = "alert alert-warning small py-2 px-3 mb-0",
          bs_icon("exclamation-triangle-fill", class = "me-1"),
          "No se encontraron registros para esta especie en Costa Rica."
        )
      }
    })

    # Validar y disparar
    observeEvent(input$btn_modelar, {
      especie <- trimws(input$especie)
      if (nchar(especie) == 0) {
        showNotification("Ingresá el nombre de la especie.",
                         type = "warning", duration = 4)
        return()
      }

      # Mostrar spinner de descarga
      shinyjs::show("spinner_descarga")

      # Resetear estado anterior
      estado$registros_sf        <- NULL
      estado$registros_listos    <- NULL
      estado$modelo_ajustado     <- NULL
      estado$prediccion_sf       <- NULL
      estado$pred_futuro_sf      <- NULL
      estado$aoa_sf              <- NULL
      estado$error_sin_registros <- FALSE
      estado$n_registros_modelo  <- NULL
      estado$n_removidos         <- NULL
      estado$n_hex_pres          <- NULL
      estado$n_hex_aus           <- NULL

      # Guardar parámetros en estado global
      estado$resolucion <- input$resolucion
      estado$algoritmo  <- .ALGORITMO
      estado$trigger_modelar <- Sys.time()
    })

    # Ocultar spinner cuando lleguen los registros
    observeEvent(estado$registros_listos, ignoreNULL = TRUE, {
      shinyjs::hide("spinner_descarga")
    })

    # Retornar reactivos — misma interfaz que antes
    # para no romper mod_registros ni mod_modelo
    list(
      especie      = reactive(input$especie),
      resolucion   = reactive(input$resolucion),
      algoritmo    = reactive(.ALGORITMO),
      limite       = reactive(.LIMITE),
      n_pseudoabs  = reactive(.N_PSEUDOABS),
      buffer_k     = reactive(.BUFFER_K),
      src_gbif     = reactive(TRUE),
      src_inat     = reactive(TRUE),
      src_bdcr     = reactive(TRUE),
      btn_modelar  = reactive(input$btn_modelar)
    )
  })
}

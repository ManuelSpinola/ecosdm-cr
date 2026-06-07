# ============================================================
# mod_registros.R
# Descarga de registros GBIF / iNaturalist / BiodataCR
# y visualización en mapa leaflet
# OVS-CR · ICOMVIS · UNA
# ============================================================

mod_registros_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "p-3",
    layout_columns(
      col_widths = c(4, 8),
      fill       = FALSE,

      # Panel izquierdo
      div(
        card(
          class = "mb-3",
          card_header(bs_icon("info-circle", class = "me-1"),
                      "Resumen"),
          card_body(uiOutput(ns("resumen_registros")))
        ),
        card(
          card_header(bs_icon("table", class = "me-1"),
                      "Registros por fuente"),
          card_body(uiOutput(ns("tabla_fuentes")))
        ),
        br(),
        downloadButton(ns("dl_registros"), "Descargar CSV",
                       class = "btn-outline-primary btn-sm w-100")
      ),

      # Panel derecho — mapa
      card(
        card_header(bs_icon("map", class = "me-1"),
                    "Mapa de ocurrencias"),
        card_body(
          class = "p-0",
          leaflet::leafletOutput(ns("mapa_registros"), height = "520px")
        )
      )
    )
  )
}

mod_registros_server <- function(id, estado, sidebar_vals) {
  moduleServer(id, function(input, output, session) {

    # Mapa base
    output$mapa_registros <- leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
        leaflet::setView(lng = -84.0, lat = 9.9, zoom = 7)
    })

    # Descargar registros al presionar "Modelar"
    observeEvent(sidebar_vals$btn_modelar(), {
      especie <- trimws(sidebar_vals$especie())
      req(nchar(especie) > 0)

      providers_sel <- c(
        if (sidebar_vals$src_gbif()) "gbif",
        if (sidebar_vals$src_inat()) "inat",
        if (sidebar_vals$src_bdcr()) "biodatacr"
      )
      if (length(providers_sel) == 0) return()

      withProgress(message = paste("Descargando registros de", especie, "…"), {
        tryCatch({


          recs <- h3sdm::h3sdm_get_records(
            species           = especie,
            aoi_sf            = h3sdm::cr_outline_c,
            providers         = providers_sel,
            limit             = sidebar_vals$limite(),
            remove_duplicates = TRUE,
            date              = c("1990-01-01", as.character(Sys.Date()))
          )

          if (is.null(recs) || nrow(recs) == 0) {
            showNotification(
              paste0("No se encontraron registros de '", especie,
                     "' en Costa Rica."),
              type = "warning", duration = 6)
            estado$registros_sf <- NULL
            return()
          }

          estado$registros_sf <- recs

          # Actualizar mapa
          coords <- sf::st_coordinates(sf::st_transform(recs, 4326))
          bbox   <- sf::st_bbox(sf::st_transform(recs, 4326))
          leaflet::leafletProxy(session$ns("mapa_registros")) |>
            leaflet::clearMarkers() |>
            leaflet::addCircleMarkers(
              lng         = coords[, 1],
              lat         = coords[, 2],
              radius      = 5,
              color       = "#a31e32",
              fillColor   = "#00A651",
              fillOpacity = 0.8,
              weight      = 1,
              popup       = if ("source" %in% names(recs))
                              paste0("<b>", especie, "</b><br>",
                                     sf::st_drop_geometry(recs)$source)
                            else especie
            ) |>
            leaflet::addPolygons(
              data        = sf::st_transform(h3sdm::cr_outline_c, 4326),
              color       = "#003865",
              fillOpacity = 0,
              weight      = 1.5
            ) |>
            leaflet::fitBounds(bbox[["xmin"]], bbox[["ymin"]],
                               bbox[["xmax"]], bbox[["ymax"]])

          showNotification(
            paste0(nrow(recs), " registros descargados."),
            type = "message", duration = 4)

        }, error = function(e) {
          showNotification(paste("Error al descargar registros:", conditionMessage(e)),
                           type = "error", duration = 8)
        })
      })
    })

    # Resumen
    output$resumen_registros <- renderUI({
      recs <- estado$registros_sf
      if (is.null(recs)) {
        return(p(class = "small text-muted mb-0",
                 bs_icon("exclamation-circle", class = "me-1"),
                 "Presioná 'Ver distribución' para descargar registros."))
      }
      div(
        class = "alert alert-success small py-2 px-3 mb-0",
        bs_icon("check-circle-fill", class = "me-1"),
        strong(nrow(recs)), " registros en Costa Rica"
      )
    })

    # Tabla por fuente
    output$tabla_fuentes <- renderUI({
      recs <- estado$registros_sf
      if (is.null(recs) || !"source" %in% names(recs)) return(NULL)
      df <- as.data.frame(table(sf::st_drop_geometry(recs)$source))
      names(df) <- c("Fuente", "Registros")
      tags$table(
        class = "table table-sm small mb-0",
        tags$thead(
          style = "background:#a31e32; color:#fff;",
          tags$tr(lapply(names(df), tags$th))
        ),
        tags$tbody(
          apply(df, 1, function(r) tags$tr(lapply(r, tags$td)))
        )
      )
    })

    # Descarga
    output$dl_registros <- downloadHandler(
      filename = function() {
        paste0("registros_", gsub(" ", "_", estado$registros_sf |>
                                    attr("species") %||% "especie"),
               "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(estado$registros_sf)
        write.csv(sf::st_drop_geometry(estado$registros_sf),
                  file, row.names = FALSE)
      }
    )
  })
}

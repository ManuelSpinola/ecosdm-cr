# ============================================================
# mod_mapas.R
# OVS-CR · ICOMVIS · UNA
# ============================================================

.etiquetas_hab  <- c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto")
.colores_hab    <- c("#d73027", "#fc8d59", "#fee08b", "#91cf60", "#1a9850")
.breaks_hab     <- c(0, 0.2, 0.4, 0.6, 0.8, 1)

.cat_habitat <- function(vals) {
  factor(
    cut(vals, breaks = .breaks_hab,
        labels = .etiquetas_hab, include.lowest = TRUE),
    levels = .etiquetas_hab, ordered = TRUE
  )
}

.mapa_vacio <- function() {
  leaflet::leaflet() |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
    leaflet::setView(lng = -84.0, lat = 9.9, zoom = 7)
}

# ── UI ────────────────────────────────────────────────────────
mod_mapas_ui <- function(id, tipo = "presente") {
  ns <- NS(id)

  if (tipo == "presente") {
    div(class = "p-3",
      uiOutput(ns("resumen")), br(),
      layout_columns(col_widths = c(6, 6),
        card(card_header(bs_icon("palette", class = "me-1"), "Idoneidad continua (0-1)"),
             card_body(class = "p-0", leaflet::leafletOutput(ns("mapa_cont"), height = "520px"))),
        card(card_header(bs_icon("layers", class = "me-1"), "Categorias de habitat"),
             card_body(class = "p-0", leaflet::leafletOutput(ns("mapa_cat"), height = "520px")))
      ), br(),
      layout_columns(col_widths = c(3, 3),
        downloadButton(ns("dl_cont"), "Descargar continuo (.gpkg)", class = "btn-outline-primary btn-sm w-100"),
        downloadButton(ns("dl_cat"),  "Descargar categorico (.gpkg)", class = "btn-outline-secondary btn-sm w-100")
      )
    )

  } else if (tipo == "futuro") {
    div(class = "p-3",
      uiOutput(ns("resumen")), br(),
      layout_columns(col_widths = c(6, 6),
        card(card_header(bs_icon("palette", class = "me-1"), "Idoneidad futura continua (0-1)"),
             card_body(class = "p-0", leaflet::leafletOutput(ns("mapa_cont"), height = "520px"))),
        card(card_header(bs_icon("layers", class = "me-1"), "Categorias de habitat futuro"),
             card_body(class = "p-0", leaflet::leafletOutput(ns("mapa_cat"), height = "520px")))
      ), br(),
      layout_columns(col_widths = c(3, 3),
        downloadButton(ns("dl_cont"), "Descargar continuo (.gpkg)", class = "btn-outline-primary btn-sm w-100"),
        downloadButton(ns("dl_cat"),  "Descargar categorico (.gpkg)", class = "btn-outline-secondary btn-sm w-100")
      )
    )

  } else {
    sufijo <- if (tipo == "aoa_futuro") " futuro" else ""
    div(class = "p-3",
      uiOutput(ns("resumen")), br(),
      layout_columns(col_widths = c(6, 6),
        card(card_header(bs_icon("shield-check", class = "me-1"), paste0("Area de Aplicabilidad (AOA)", sufijo)),
             card_body(class = "p-0", leaflet::leafletOutput(ns("mapa_aoa_bin"), height = "480px"))),
        card(card_header(bs_icon("graph-up", class = "me-1"), paste0("Indice de Disimilaridad (DI)", sufijo)),
             card_body(class = "p-0", leaflet::leafletOutput(ns("mapa_di"), height = "480px")))
      ), br(),
      layout_columns(col_widths = c(6, 6),
        card(card_header(bs_icon("palette", class = "me-1"), paste0("Idoneidad dentro del AOA", sufijo, " (continua)")),
             card_body(class = "p-0", leaflet::leafletOutput(ns("mapa_aoa_cont"), height = "480px"))),
        card(card_header(bs_icon("layers", class = "me-1"), paste0("Categorias dentro del AOA", sufijo)),
             card_body(class = "p-0", leaflet::leafletOutput(ns("mapa_aoa_cat"), height = "480px")))
      ), br(),
      downloadButton(ns("dl_aoa"), paste0("Descargar AOA", sufijo, " (.gpkg)"), class = "btn-outline-primary btn-sm")
    )
  }
}

# ── Server ────────────────────────────────────────────────────
mod_mapas_server <- function(id, estado, tipo = "presente") {
  moduleServer(id, function(input, output, session) {

    if (tipo %in% c("presente", "futuro")) {

      pred_rv <- reactive({
        if (tipo == "presente") estado$prediccion_sf
        else                    estado$pred_futuro_sf
      })

      titulo_cont <- if (tipo == "presente") "Idoneidad presente" else "Idoneidad 2061-2080"
      titulo_cat  <- if (tipo == "presente") "Habitat presente"   else "Habitat 2061-2080"

      output$resumen <- renderUI({
        p <- pred_rv()
        if (is.null(p)) {
          return(div(class = "alert alert-light small py-2 px-3",
            bs_icon("hourglass-split", class = "me-1"),
            if (tipo == "presente") "Presiona 'Ver distribucion' para generar el mapa."
            else "Requiere covariables CHELSA futuras pre-procesadas en data/."))
        }
        vals <- p$prediction
        div(class = "alert alert-success small py-2 px-3",
          bs_icon("check-circle-fill", class = "me-1"),
          strong(nrow(p)), " hexagonos | Media: ",
          strong(round(mean(vals, na.rm = TRUE), 3)), " | Rango: ",
          strong(round(min(vals, na.rm = TRUE), 3)), " - ",
          strong(round(max(vals, na.rm = TRUE), 3)))
      })

      output$mapa_cont <- leaflet::renderLeaflet({
        p <- pred_rv()
        if (is.null(p)) return(.mapa_vacio())
        p_vis <- suppressWarnings(sf::st_cast(p, "POLYGON")) |> sf::st_transform(4326)
        bbox <- sf::st_bbox(p_vis)
        pal_cont <- leaflet::colorNumeric("inferno", domain = c(0, 1), reverse = TRUE)
        leaflet::leaflet() |>
          leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
          leafgl::addGlPolygons(data = p_vis, fillColor = ~pal_cont(prediction),
            fillOpacity = 0.85, color = "transparent", weight = 0) |>
          leaflet::addLegend(position = "bottomright", pal = pal_cont,
            values = c(0, 1), title = titulo_cont, opacity = 0.8) |>
          leaflet::fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
      })

      output$mapa_cat <- leaflet::renderLeaflet({
        p <- pred_rv()
        if (is.null(p)) return(.mapa_vacio())
        p_vis <- suppressWarnings(sf::st_cast(p, "POLYGON")) |> sf::st_transform(4326)
        p_vis$categoria <- .cat_habitat(p_vis$prediction)
        bbox <- sf::st_bbox(p_vis)
        pal_cat <- leaflet::colorFactor(palette = .colores_hab, levels = .etiquetas_hab, ordered = TRUE)
        leaflet::leaflet() |>
          leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
          leafgl::addGlPolygons(data = p_vis, fillColor = ~pal_cat(categoria),
            fillOpacity = 0.85, color = "transparent", weight = 0) |>
          leaflet::addLegend(position = "bottomright", colors = .colores_hab,
            labels = .etiquetas_hab, title = titulo_cat, opacity = 0.8) |>
          leaflet::fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
      })

      output$dl_cont <- downloadHandler(
        filename = function() paste0("dist_", tipo, "_continua_", Sys.Date(), ".gpkg"),
        content  = function(file) { req(pred_rv()); sf::st_write(pred_rv(), file, delete_dsn = TRUE, quiet = TRUE) }
      )
      output$dl_cat <- downloadHandler(
        filename = function() paste0("dist_", tipo, "_categorica_", Sys.Date(), ".gpkg"),
        content  = function(file) {
          req(pred_rv()); p <- pred_rv(); p$categoria <- .cat_habitat(p$prediction)
          sf::st_write(p, file, delete_dsn = TRUE, quiet = TRUE)
        }
      )

    } else { # AOA

      aoa_rv <- reactive({
        if (tipo == "aoa_futuro") estado$aoa_futuro_sf
        else                      estado$aoa_sf
      })

      msg_espera <- if (tipo == "aoa_futuro")
        "El AOA futuro se calcula junto con la distribucion futura."
      else
        "El AOA se calcula automaticamente junto con la prediccion presente."

      dl_sufijo <- if (tipo == "aoa_futuro") "futuro" else "presente"

      output$resumen <- renderUI({
        r <- aoa_rv()
        if (is.null(r)) {
          return(div(class = "alert alert-light small py-2 px-3",
            bs_icon("hourglass-split", class = "me-1"), msg_espera))
        }
        n_in  <- sum(r$AOA == 1L, na.rm = TRUE)
        n_out <- sum(r$AOA == 0L, na.rm = TRUE)
        pct   <- round(100 * n_out / nrow(r), 1)
        div(class = "alert alert-info small py-2 px-3",
          bs_icon("shield-check", class = "me-1"),
          strong(nrow(r)), " hexagonos | Dentro del AOA: ", strong(n_in),
          " | Fuera: ", strong(n_out), " (", strong(pct), "%)")
      })

      output$mapa_aoa_bin <- leaflet::renderLeaflet({
        r <- aoa_rv()
        if (is.null(r)) return(.mapa_vacio())
        r_vis <- suppressWarnings(sf::st_cast(r, "POLYGON")) |> sf::st_transform(4326)
        bbox <- sf::st_bbox(r_vis)
        pal_aoa <- leaflet::colorFactor(palette = c("#d9d9d9", "#2166ac"),
          levels = c(0L, 1L), na.color = "#d9d9d9")
        leaflet::leaflet() |>
          leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
          leafgl::addGlPolygons(data = r_vis, fillColor = ~pal_aoa(AOA),
            fillOpacity = 0.85, color = "transparent", weight = 0) |>
          leaflet::addLegend(position = "bottomright",
            colors = c("#2166ac", "#d9d9d9"), labels = c("Dentro del AOA", "Fuera del AOA"),
            title = "AOA", opacity = 0.8) |>
          leaflet::fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
      })

      output$mapa_di <- leaflet::renderLeaflet({
        r <- aoa_rv()
        if (is.null(r)) return(.mapa_vacio())
        r_vis <- suppressWarnings(sf::st_cast(r, "POLYGON")) |> sf::st_transform(4326)
        bbox <- sf::st_bbox(r_vis)
        pal_di <- leaflet::colorNumeric("YlOrRd", domain = r_vis$DI, na.color = "#d9d9d9")
        leaflet::leaflet() |>
          leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
          leafgl::addGlPolygons(data = r_vis, fillColor = ~pal_di(DI),
            fillOpacity = 0.85, color = "transparent", weight = 0) |>
          leaflet::addLegend(position = "bottomright", pal = pal_di,
            values = r_vis$DI, title = "DI", opacity = 0.8) |>
          leaflet::fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
      })

      output$mapa_aoa_cont <- leaflet::renderLeaflet({
        r <- aoa_rv()
        if (is.null(r)) return(.mapa_vacio())
        r_vis <- suppressWarnings(sf::st_cast(r, "POLYGON")) |> sf::st_transform(4326)
        bbox <- sf::st_bbox(r_vis)
        pal_cont <- leaflet::colorNumeric("inferno", domain = c(0, 1),
          reverse = TRUE, na.color = "#d9d9d9")
        leaflet::leaflet() |>
          leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
          leafgl::addGlPolygons(data = r_vis, fillColor = ~pal_cont(prediction_aoa),
            fillOpacity = 0.85, color = "transparent", weight = 0) |>
          leaflet::addLegend(position = "bottomright", pal = pal_cont,
            values = c(0, 1), title = "Idoneidad (AOA)", opacity = 0.8) |>
          leaflet::fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
      })

      output$mapa_aoa_cat <- leaflet::renderLeaflet({
        r <- aoa_rv()
        if (is.null(r)) return(.mapa_vacio())
        r_vis <- suppressWarnings(sf::st_cast(r, "POLYGON")) |> sf::st_transform(4326)
        r_vis$categoria_aoa <- .cat_habitat(r_vis$prediction_aoa)
        bbox <- sf::st_bbox(r_vis)
        pal_cat <- leaflet::colorFactor(palette = .colores_hab, levels = .etiquetas_hab,
          ordered = TRUE, na.color = "#d9d9d9")
        leaflet::leaflet() |>
          leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
          leafgl::addGlPolygons(data = r_vis, fillColor = ~pal_cat(categoria_aoa),
            fillOpacity = 0.85, color = "transparent", weight = 0) |>
          leaflet::addLegend(position = "bottomright",
            colors = c(.colores_hab, "#d9d9d9"),
            labels = c(.etiquetas_hab, "Fuera del AOA"),
            title = "Habitat (AOA)", opacity = 0.8) |>
          leaflet::fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
      })

      output$dl_aoa <- downloadHandler(
        filename = function() paste0("aoa_", dl_sufijo, "_", Sys.Date(), ".gpkg"),
        content  = function(file) { req(aoa_rv()); sf::st_write(aoa_rv(), file, delete_dsn = TRUE, quiet = TRUE) }
      )
    }
  })
}

# ============================================================
# utils_data.R
# Carga de covariables CHELSA pre-procesadas por resolución H3
# OVS-CR · ICOMVIS · UNA
# ============================================================

# ── Rutas de datos ───────────────────────────────────────────
DATA_DIR <- "data"

# Mapeo resolución → archivo de covariables
covariables_paths <- list(
  actual = list(
    "6" = file.path(DATA_DIR, "bio_chelsa_presente_no_cor_cr_5367_h6.gpkg"),
    "7" = file.path(DATA_DIR, "bio_chelsa_presente_no_cor_cr_5367_h7.gpkg"),
    "8" = file.path(DATA_DIR, "bio_chelsa_presente_no_cor_cr_5367_h8.gpkg")
  ),
  futuro = list(
    "6" = file.path(DATA_DIR, "bio_chelsa_futuro_cr_5367_h6.gpkg"),
    "7" = file.path(DATA_DIR, "bio_chelsa_futuro_cr_5367_h7.gpkg"),
    "8" = file.path(DATA_DIR, "bio_chelsa_futuro_cr_5367_h8.gpkg")
  )
)

# ── Cache en memoria (se carga una sola vez por sesión) ─────
.cov_cache <- new.env(parent = emptyenv())

#' Cargar covariables para una resolución dada
#'
#' @param resolucion character "6", "7" o "8"
#' @param escenario  character "actual" o "futuro"
#' @return sf con h3_address + variables (o NULL si el archivo no existe)
cargar_covariables <- function(resolucion, escenario = "actual") {
  key <- paste0(escenario, "_res", resolucion)

  if (exists(key, envir = .cov_cache)) {
    return(get(key, envir = .cov_cache))
  }

  path <- covariables_paths[[escenario]][[as.character(resolucion)]]

  if (is.null(path) || !file.exists(path)) {
    warning(sprintf(
      "[utils_data] Archivo no encontrado: %s\n  Prepará el gpkg con h3sdm_extract_num() y colocálo en data/",
      path %||% "(ruta no definida)"
    ))
    return(NULL)
  }

  dat <- sf::st_read(path, quiet = TRUE)
  assign(key, dat, envir = .cov_cache)
  dat
}

#' Verificar disponibilidad de covariables
#' @return data.frame con estado de cada archivo
verificar_covariables <- function() {
  res <- do.call(rbind, lapply(c("6", "7", "8"), function(r) {
    data.frame(
      resolucion = r,
      actual     = file.exists(covariables_paths$actual[[r]]),
      futuro     = file.exists(covariables_paths$futuro[[r]]),
      stringsAsFactors = FALSE
    )
  }))
  res
}

#' Operador %||% (null coalescing)
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ── Info de resoluciones ─────────────────────────────────────
info_resoluciones <- data.frame(
  res   = c("6", "7", "8"),
  label = c(
    "6 — 36.1 km² (paisaje local)",
    "7 — 5.2 km² (SDM fino ★)",
    "8 — 0.74 km² (escala de sitio)"
  ),
  area  = c("36.1 km²", "5.2 km²", "0.74 km²"),
  stringsAsFactors = FALSE
)

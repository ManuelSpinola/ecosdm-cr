library(bslib)
# ============================================================
# helpers.R — Configuración compartida entre módulos
# Explorador de Especies Terrestres · Costa Rica
# Paleta institucional UNA / ICOMVIS
# OVS · EcoSuite · Manuel Spínola · ICOMVIS · UNA
# ============================================================

# ── Paleta de colores institucional UNA/ICOMVIS ──────────────
colores <- list(
  primario    = "#a31e32",   # rojo UNA
  secundario  = "#003865",   # azul ICOMVIS
  acento      = "#00A651",   # verde
  peligro     = "#B71234",   # rojo oscuro
  exito       = "#00A651",   # verde
  advertencia = "#FFC107",   # amarillo
  fondo       = "#F8F4F4",   # fondo cálido (tono rosado muy suave)
  fondo_card  = "#FDF0F0",   # fondo cards
  texto       = "#2C2C2C",
  borde       = "#E8C8C8",
  navbar      = "#a31e32",   # navbar rojo UNA

  tableau = c(
    "#a31e32", "#003865", "#00A651", "#FFC107",
    "#5FA2CE", "#C85200", "#7B848F", "#A3CDE9"
  )
)

# ── Tema bslib ───────────────────────────────────────────────
tema_app <- bs_theme(
  version      = 5,
  bootswatch   = NULL,
  bg           = colores$fondo,
  fg           = colores$texto,
  primary      = colores$primario,
  secondary    = colores$secundario,
  success      = colores$exito,
  danger       = colores$peligro,
  warning      = colores$advertencia,
  base_font    = font_google("Nunito"),
  heading_font = font_google("Nunito", wght = 700),
  code_font    = font_google("Fira Mono")
) |>
  bs_add_rules("
  /* ── Navbar ── */
  .navbar { background-color: #a31e32 !important; }
  .navbar-brand, .navbar .nav-link { color: #ffffff !important; }
  .navbar .nav-link.active { border-bottom: 2px solid #FFC107; }

  /* ── Tabs activos ── */
  .nav-tabs .nav-link.active {
    background-color: #a31e32 !important;
    color: #ffffff !important;
    border-top-color: #a31e32 !important;
    border-left-color: #a31e32 !important;
    border-right-color: #a31e32 !important;
    border-bottom-color: transparent !important;
    font-weight: 600 !important;
  }
  .nav-tabs .nav-link:not(.active):hover {
    background-color: #F8F0F0 !important;
    color: #a31e32 !important;
  }

  /* ── Botón primario ── */
  .btn-primary {
    background-color: #a31e32 !important;
    border-color: #a31e32 !important;
    color: #ffffff !important;
  }
  .btn-primary:hover {
    background-color: #8a1929 !important;
    border-color: #8a1929 !important;
  }

  /* ── Cards ── */
  .card > .card-header {
    background-color: #EDD5D8;
    color: #a31e32;
    font-weight: 700;
    border-bottom: none;
  }

  /* ── Sidebar ── */
  .bslib-sidebar-layout > .sidebar {
    background-color: #F8F4F4 !important;
    border-right: 1px solid #E8C8C8;
  }

  /* ── Título explorador en sidebar ── */
  .titulo-explorador {
    color: #a31e32;
    font-size: 0.95rem;
    font-weight: 700;
    line-height: 1.3;
    text-align: center;
  }

  /* ── Alertas con tono cálido ── */
  .alert-light {
    background-color: #FDF0F0;
    border-color: #E8C8C8;
  }
")

# ── Escalas ggplot2 ──────────────────────────────────────────
scale_fill_ovs <- function(...) {
  ggplot2::scale_fill_manual(values = colores$tableau, ...)
}
scale_color_ovs <- function(...) {
  ggplot2::scale_color_manual(values = colores$tableau, ...)
}

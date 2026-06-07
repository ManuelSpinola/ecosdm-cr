# OVS-CR · Explorador de Especies Terrestres · Costa Rica
**ICOMVIS · Universidad Nacional de Costa Rica**  
Manuel Spínola · Versión 2.0

---

## Estructura del proyecto

```
ovs_cr_sdm/
├── app.R                    # Punto de entrada
├── R/
│   ├── helpers.R            # Paleta de colores, tema bslib, escalas ggplot2
│   ├── utils_data.R         # Carga de covariables CHELSA pre-procesadas
│   ├── mod_sidebar.R        # Especie + resolución H3 + botón Ver distribución
│   ├── mod_registros.R      # Descarga GBIF / iNaturalist / BiodataCR
│   ├── mod_modelo.R         # PA + ajuste + predicción presente/futura + AOA (sin UI)
│   ├── mod_mapas.R          # Mapas leaflet + leafgl (presente / futuro / AOA / AOA futuro)
│   └── mod_metricas.R       # Métricas, ROC, importancia de variables, PDP
├── data/
│   ├── chelsa_actual_res6.gpkg    # Variables bioclimáticas CHELSA actuales, res H3 = 6
│   ├── chelsa_actual_res7.gpkg    # ídem res 7
│   ├── chelsa_actual_res8.gpkg    # ídem res 8
│   ├── chelsa_futuro_res6.gpkg    # Variables bioclimáticas CHELSA futuras SSP5-8.5, res 6
│   ├── chelsa_futuro_res7.gpkg    # ídem res 7
│   └── chelsa_futuro_res8.gpkg    # ídem res 8
└── www/
    └── logo_ICOMVIS_circular.png
```

---

## Decisión metodológica: solo variables bioclimáticas CHELSA

Esta app usa **exclusivamente variables bioclimáticas CHELSA no correlacionadas**
como predictores. Estas decisiones son intencionales:

### ¿Por qué solo variables bioclimáticas?

**1. Consistencia presente–futuro**  
Variables de paisaje como cobertura boscosa, NDVI o métricas de fragmentación
solo están disponibles para el presente. No existen proyecciones confiables de
estas variables bajo escenarios de cambio climático futuro (SSP5-8.5 2061–2080).
Usar variables de paisaje en el modelo presente impediría generar predicciones
futuras coherentes.

**2. Comparabilidad entre especies**  
Al usar el mismo conjunto de predictores bioclimáticos para todas las especies,
los resultados son comparables entre sí y el flujo de trabajo es completamente
automatizable — el usuario solo escoge la especie y la resolución.

### ¿Por qué CHELSA y no WorldClim?

CHELSA (Climatologies at High resolution for the Earth's Land Surface Areas)
ofrece varias ventajas sobre WorldClim para Costa Rica y la región tropical:

**Mejor representación de precipitación en zonas montañosas**  
CHELSA usa un downscaling basado en modelos atmosféricos que captura mejor
los gradientes orográficos abruptos de la Cordillera de Talamanca y la
diferencia entre la vertiente Pacífica y Caribeña — zonas donde WorldClim
tiende a subestimar la precipitación por sus limitaciones de interpolación espacial.

**Consistencia presente–futuro**  
Las proyecciones futuras de CHELSA (CHELSA-CMIP6) usan el mismo método de
downscaling que el presente, garantizando consistencia metodológica entre
ambos escenarios. WorldClim futuro y presente usan métodos distintos.

**Cobertura temporal y actualización**  
CHELSA cubre 1981–presente con actualizaciones periódicas, mientras que
WorldClim 2.1 tiene una línea base fija (1970–2000).

> Karger, D.N. et al. (2017). Climatologies at high resolution for the Earth's
> land surface areas. *Scientific Data*, 4, 170122.
> [doi:10.1038/sdata.2017.122](https://doi.org/10.1038/sdata.2017.122)

**Fuentes de descarga:**
- Presente: [chelsa-climate.org](https://chelsa-climate.org) → CHELSA-BIOCLIM+
- Futuro SSP5-8.5 2061–2080: mismo sitio → CHELSA-CMIP6 → ensamble de GCMs

### Filtro de correlación

Las variables bioclimáticas deben ser filtradas por correlación antes de
guardarlas en los `.gpkg`. Se recomienda usar `filter_collinear()` con un
umbral de correlación de Pearson r < 0.7.

---

## Preparar las covariables CHELSA

Los archivos `.gpkg` en `data/` deben prepararse antes de desplegar la app.
El formato GeoPackage (`.gpkg`) es el único formato soportado.

Ejemplo completo para resolución 7:

```r
library(h3sdm)
library(sf)
library(terra)
library(tidysdm)  # para filter_collinear()

# 1. Grilla H3 para Costa Rica continental (H3 genera en WGS84)
cr_grid_7 <- h3sdm_get_grid(h3sdm::cr_outline_c, res = 7)

# 2. Transformar grilla a EPSG:5367 (CRTM05 — CRS oficial de Costa Rica)
#    Todo el procesamiento y los .gpkg finales deben estar en 5367.
#    Solo se transforma a 4326 en la app, justo antes de renderizar en leaflet.
cr_grid_7_5367 <- sf::st_transform(cr_grid_7, 5367)

# 3. Cargar rasters CHELSA (variables bioclimáticas actuales)
#    Los rasters deben estar reprojectados a EPSG:5367 antes de extraer.
bio_actual <- terra::rast("ruta/a/chelsa_bio_actual.tif")
bio_actual <- terra::project(bio_actual, "EPSG:5367")

# 4. Filtrar variables correlacionadas (r < 0.7)
vars_no_cor <- filter_collinear(bio_actual, cutoff = 0.7, method = "cor_caret")
bio_actual_nc <- bio_actual[[vars_no_cor]]

# 5. Extraer variables dentro de cada hexágono (media por hexágono)
cov_actual <- h3sdm_extract_num(bio_actual_nc, cr_grid_7_5367)
# Verificar CRS: debe ser EPSG:5367
sf::st_crs(cov_actual)$epsg  # → 5367

# 6. Guardar como GeoPackage en EPSG:5367
sf::st_write(cov_actual, "data/chelsa_actual_res7.gpkg",
             delete_dsn = TRUE)

# 7. Repetir para futuro — mismas variables, mismo CRS
bio_futuro <- terra::rast("ruta/a/chelsa_bio_futuro_ssp585_2061_2080.tif")
bio_futuro <- terra::project(bio_futuro, "EPSG:5367")
bio_futuro_nc <- bio_futuro[[vars_no_cor]]  # exactamente las mismas variables
cov_futuro <- h3sdm_extract_num(bio_futuro_nc, cr_grid_7_5367)
sf::st_write(cov_futuro, "data/chelsa_futuro_res7.gpkg",
             delete_dsn = TRUE)

# 8. Repetir para resoluciones 6 y 8
```

> **Importante:**
> - Todos los `.gpkg` deben estar en **EPSG:5367**. La app no hace
>   transformaciones de CRS en tiempo de ejecución — solo transforma
>   a 4326 justo antes de renderizar en leaflet.
> - Los archivos actual y futuro deben tener exactamente las mismas
>   columnas (mismas variables bioclimáticas). El modelo se entrena
>   con las variables actuales y predice sobre las futuras.
> - El filtro de correlación `filter_collinear()` se aplica **solo una vez**
>   sobre las variables actuales. Las mismas variables seleccionadas
>   se usan para el escenario futuro.

---

## Paquetes requeridos

```r
install.packages(c(
  "shiny", "bslib", "bsicons", "shinyjs",
  "leaflet", "leafgl", "leaflet.extras",
  "sf", "terra", "dplyr", "rlang",
  "tidymodels", "parsnip", "recipes", "tune",
  "spatialsample", "yardstick",
  "DALEX", "ingredients",
  "ggplot2", "DT"
))

# Paquetes del ecosistema OVS/h3sdm
remotes::install_github("mspinola/h3sdm")
```

---

## Desplegar en Posit Connect Cloud

```r
library(rsconnect)

# Configurar cuenta (solo la primera vez)
rsconnect::setAccountInfo(
  name   = "tu_usuario",
  token  = "TU_TOKEN",
  secret = "TU_SECRET"
)

# Desplegar
rsconnect::deployApp(
  appDir      = "ruta/a/ovs_cr_sdm",
  appName     = "explorador-especies-cr",
  appTitle    = "Explorador de Especies Terrestres · Costa Rica",
  forceUpdate = TRUE
)
```

### Nota sobre el tamaño de los archivos `data/`
Posit Connect Cloud tiene un límite de tamaño de bundle. Si los `.gpkg`
son grandes (> 1 GB), considerá:
- Almacenarlos en **Posit Connect Pins** (`pins::pin_write()`) y
  cargarlos al inicio desde `utils_data.R`.
- O usar una base de datos PostGIS accesible desde el servidor.

---

## Diferencias respecto a la versión original (shinyapps.io)

| Aspecto | Versión anterior | Versión 2.0 |
|---|---|---|
| Framework | Quarto Dashboard + `server: shiny` | Shiny puro + bslib |
| Mapas | `plotOutput` + ggplot2/tidyterra | `leafletOutput` + `leafgl` |
| Hexágonos | mapview (lento con >10k polígonos) | leafgl (WebGL, muy rápido) |
| Covariables | Extraídas en tiempo real desde rasters | Pre-procesadas en `.gpkg` |
| Variables | Bioclimáticas + paisaje (inconsistente) | Solo bioclimáticas no correlacionadas |
| Rasters en servidor | Sí (terra/tidyterra) | No — sin rasters en tiempo de ejecución |
| AOA | No incluido | AOA presente + AOA futuro (4 mapas c/u) |
| Modelos | Solo GAM hardcodeado | Random Forest (fijo, óptimo para SDM) |
| Estructura | 1 archivo .qmd | Módulos separados + helpers.R |
| Público objetivo | Técnico | General — solo especie + resolución |

---

## EcoSuite

Esta app forma parte de **EcoSuite**, una colección de aplicaciones web
para visualización y consulta de biodiversidad, desarrolladas por ICOMVIS-UNA.
A diferencia de **StatSuite** (herramientas analíticas con estructura de paquete R),
las apps de EcoSuite son productos de visualización standalone orientados
al público general — el usuario no necesita conocimientos de R ni de SDM.

# ============================================================
# mod_metricas.R
# Métricas de validación cruzada, curva ROC,
# importancia de variables (DALEX) y PDP
# OVS-CR · ICOMVIS · UNA
# ============================================================

mod_metricas_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "p-3",

    uiOutput(ns("aviso_modelo")),

    layout_columns(
      col_widths = c(6, 6),
      fill = FALSE,

      # Métricas CV
      card(
        class = "mb-3",
        card_header(bs_icon("bar-chart-steps", class = "me-1"),
                    "Métricas de validación cruzada"),
        card_body(
          uiOutput(ns("tabla_metricas")),
          br(),
          downloadButton(ns("dl_metricas"), "Descargar métricas",
                         class = "btn-outline-primary btn-sm w-100")
        )
      ),

      # Curva ROC
      card(
        class = "mb-3",
        card_header(bs_icon("graph-up", class = "me-1"),
                    "Curva ROC"),
        card_body(
          plotOutput(ns("plot_roc"), height = "280px")
        )
      )
    ),

    # Importancia
    card(
      class = "mb-3",
      card_header(bs_icon("bar-chart-steps", class = "me-1"),
                  "Importancia de variables (permutación · DALEX)"),
      card_body(
        layout_columns(
          col_widths = c(4, 4), fill = FALSE,
          actionButton(ns("btn_importancia"), "Calcular importancia",
                       class = "btn-outline-primary btn-sm w-100",
                       icon = icon("calculator")),
          downloadButton(ns("dl_importancia"), "Descargar",
                         class = "btn-outline-secondary btn-sm w-100")
        ),
        br(),
        plotOutput(ns("plot_importancia"), height = "320px")
      )
    ),

    # PDP
    card(
      class = "mb-0",
      card_header(bs_icon("bezier2", class = "me-1"),
                  "Gráficos de dependencia parcial (PDP)"),
      card_body(
        layout_columns(
          col_widths = c(3, 9), fill = FALSE,
          div(
            uiOutput(ns("sel_vars_pdp")),
            br(),
            actionButton(ns("btn_pdp"), "Calcular PDP",
                         class = "btn-outline-primary btn-sm w-100",
                         icon = icon("play")),
            br(), br(),
            downloadButton(ns("dl_pdp"), "Descargar PDP",
                           class = "btn-outline-secondary btn-sm w-100")
          ),
          plotOutput(ns("plot_pdp"), height = "300px")
        )
      )
    )
  )
}

mod_metricas_server <- function(id, estado) {
  moduleServer(id, function(input, output, session) {

    explainer_obj  <- reactiveVal(NULL)
    importancia_df <- reactiveVal(NULL)

    # Aviso si no hay modelo
    output$aviso_modelo <- renderUI({
      if (is.null(estado$modelo_ajustado)) {
        div(
          class = "alert alert-light small py-2 px-3 mb-3",
          bs_icon("hourglass-split", class = "me-1"),
          "Las métricas aparecerán aquí una vez que se ajuste el modelo."
        )
      }
    })

    # Tabla de métricas CV
    output$tabla_metricas <- renderUI({
      m <- estado$modelo_ajustado; req(m)
      metrics <- m$metrics %||%
        tryCatch(tune::collect_metrics(m$cv_model), error = function(e) NULL)
      req(metrics)
      df <- as.data.frame(metrics)
      df <- df[, intersect(names(df),
                            c(".metric", "mean", "std_err", "conf_low", "conf_high"))]
      df$mean <- round(df$mean, 4)
      if ("std_err"  %in% names(df)) df$std_err  <- round(df$std_err, 4)
      names(df)[names(df) == ".metric"] <- "Métrica"
      names(df)[names(df) == "mean"]    <- "Media"
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

    # Curva ROC
    output$plot_roc <- renderPlot({
      m <- estado$modelo_ajustado; req(m)
      tryCatch({
        preds <- tune::collect_predictions(m$cv_model)
        col_pred <- intersect(c(".pred_1", ".pred_presence"), names(preds))[1]
        req(!is.na(col_pred))

        roc_df  <- yardstick::roc_curve(preds,
                                         truth = presence,
                                         !!rlang::sym(col_pred),
                                         event_level = "second")
        auc_val <- yardstick::roc_auc(preds,
                                       truth = presence,
                                       !!rlang::sym(col_pred),
                                       event_level = "second")$.estimate

        ggplot2::ggplot(roc_df,
          ggplot2::aes(x = 1 - specificity, y = sensitivity)) +
          ggplot2::geom_abline(slope = 1, intercept = 0,
                               linetype = "dashed", color = "#A3ACB9") +
          ggplot2::geom_line(color = "#a31e32", linewidth = 1.2) +
          ggplot2::annotate("text", x = 0.72, y = 0.08,
                            label = paste0("AUC = ", round(auc_val, 3)),
                            color = "#a31e32", size = 5, fontface = "bold") +
          ggplot2::labs(x = "1 - Especificidad",
                        y = "Sensibilidad",
                        title = "Curva ROC (validación cruzada espacial)") +
          ggplot2::theme_minimal(base_size = 12)
      }, error = function(e) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
                            label = paste("No disponible:", conditionMessage(e)),
                            color = "#A3ACB9", size = 4) +
          ggplot2::theme_void()
      })
    })

    # Importancia de variables
    observeEvent(input$btn_importancia, {
      m  <- estado$modelo_ajustado; req(m)
      dat <- estado$dat_rv; req(dat)

      withProgress(message = "Calculando importancia de variables…", {
        tryCatch({
          exp <- h3sdm::h3sdm_explain(m$final_model, data = dat)
          explainer_obj(exp)
          vars_pred <- setdiff(names(exp$data), c("h3_address", "x", "y", "presence"))
          imp <- DALEX::model_parts(exp, variables = vars_pred, type = "difference")
          importancia_df(imp)
          showNotification("Importancia calculada.", type = "message", duration = 3)
        }, error = function(e) {
          showNotification(paste("Error:", conditionMessage(e)),
                           type = "error", duration = 8)
        })
      })
    })

    output$plot_importancia <- renderPlot({
      imp <- importancia_df(); req(imp)
      df  <- as.data.frame(imp)
      df  <- df[df$permutation == 0 &
                  df$variable != "_baseline_" &
                  df$variable != "_full_model_", ]
      df  <- df[order(df$dropout_loss, decreasing = TRUE), ]

      ggplot2::ggplot(df,
        ggplot2::aes(x = dropout_loss,
                     y = reorder(variable, dropout_loss),
                     fill = dropout_loss)) +
        ggplot2::geom_col(show.legend = FALSE) +
        ggplot2::scale_fill_gradient(low = "#57606C", high = "#a31e32") +
        ggplot2::labs(
          x = "Pérdida por permutación",
          y = NULL,
          title = "Importancia de variables") +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
    })

    # Selector de variables para PDP
    output$sel_vars_pdp <- renderUI({
      exp <- explainer_obj()
      if (is.null(exp)) {
        return(p(class = "small text-muted",
                 "Calculá importancia primero."))
      }
      vars <- setdiff(names(exp$data), c("h3_address", "x", "y", "presence"))
      checkboxGroupInput(
        session$ns("vars_pdp"),
        label    = "Variables (máx. 3):",
        choices  = vars,
        selected = vars[1:min(2, length(vars))]
      )
    })

    observeEvent(input$btn_pdp, {
      req(explainer_obj(), input$vars_pdp)
      withProgress(message = "Calculando PDP…", {
        tryCatch({
          pdp <- ingredients::partial_dependence(
            explainer_obj(), variables = input$vars_pdp)
          output$plot_pdp <- renderPlot({
            plot(pdp) +
              ggplot2::theme_minimal(base_size = 12) +
              ggplot2::labs(title = NULL, subtitle = NULL, color = NULL) +
              ggplot2::theme(legend.position = "none")
          })
        }, error = function(e) {
          showNotification(paste("Error PDP:", conditionMessage(e)),
                           type = "error", duration = 8)
        })
      })
    })

    # Descargas
    output$dl_metricas <- downloadHandler(
      filename = function() paste0("metricas_", Sys.Date(), ".csv"),
      content  = function(file) {
        m <- estado$modelo_ajustado; req(m)
        metrics <- m$metrics %||% tune::collect_metrics(m$cv_model)
        write.csv(as.data.frame(metrics), file, row.names = FALSE)
      }
    )

    output$dl_importancia <- downloadHandler(
      filename = function() paste0("importancia_", Sys.Date(), ".csv"),
      content  = function(file) {
        req(importancia_df())
        write.csv(as.data.frame(importancia_df()), file, row.names = FALSE)
      }
    )

    output$dl_pdp <- downloadHandler(
      filename = function() paste0("pdp_", Sys.Date(), ".png"),
      content  = function(file) {
        req(explainer_obj(), input$vars_pdp)
        pdp <- ingredients::partial_dependence(
          explainer_obj(), variables = input$vars_pdp)
        p <- plot(pdp) +
          ggplot2::theme_minimal(base_size = 12) +
          ggplot2::labs(title = NULL, subtitle = NULL, color = NULL) +
          ggplot2::theme(legend.position = "none")
        ggplot2::ggsave(file, p, width = 8, height = 5, dpi = 150)
      }
    )
  })
}

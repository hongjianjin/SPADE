library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(ggplot2)

# =============================================================================
# ERCC Analyze Module  –  displays ERCC-specific analysis results after DE
# UI:     ERCC_AnalyzeUI(id)
# Server: ERCC_AnalyzeServer(id, tables)
# =============================================================================

#' ERCC Analyze UI
#'
#' Executes ERCC_AnalyzeUI.
#'
#' @param id Argument for ERCC_AnalyzeUI.
#'
#' @return Return value produced by ERCC_AnalyzeUI.
ERCC_AnalyzeUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("erccAnalyzePanel"))
  )
}

#' ERCC Analyze Server
#'
#' Executes ERCC_AnalyzeServer.
#'
#' @param id Argument for ERCC_AnalyzeServer.
#' @param tables Argument for ERCC_AnalyzeServer.
#'
#' @return Return value produced by ERCC_AnalyzeServer.
ERCC_AnalyzeServer <- function(id, tables) {
  moduleServer(id, function(input, output, session) {
    output$erccAnalyzePanel <- renderUI({
      req(isTRUE(tables$erccMode), tables$erccShift)
      ns <- session$ns
      tagList(
        fluidRow(
          box(
            width = 12, title = "ERCC Spike-in Shift Test",
            solidHeader = TRUE, collapsible = TRUE, status = "warning",
            fluidRow(
              column(6, DT::DTOutput(ns("shiftTable"))),
              column(6, plotOutput(ns("shiftPlot"), height = "300px"))
            )
          )
        ),
        fluidRow(
          box(
            width = 12, title = "ERCC Normalisation Factors (DE comparison)",
            solidHeader = TRUE, collapsible = TRUE, status = "info",
            shinycssloaders::withSpinner(DT::DTOutput(ns("erccStatDEtable")))
          )
        )
      )
    })

    # Shift test result table
    output$shiftTable <- DT::renderDT({
      req(tables$erccShift)
      shift <- tables$erccShift
      df <- data.frame(
        Metric = c(
          "Comparison", "SHIFT", "AUC", "Fold Shift", "P-value",
          "Mean (Group 1)", "Mean (Group 2)",
          "CV% (Group 1)", "CV% (Group 2)"
        ),
        Value = c(
          as.character(shift$comp),
          as.character(shift$SHIFT),
          round(shift$AUC, 4),
          round(shift$Shift_FC, 4),
          format.pval(shift$Shift_PVAL, digits = 3),
          round(shift$mean_group1, 4),
          round(shift$mean_group2, 4),
          round(shift$CV_group1, 2),
          round(shift$CV_group2, 2)
        ),
        stringsAsFactors = FALSE
      )
      DT::datatable(df,
        rownames = FALSE, class = "display compact",
        options = list(dom = "t", pageLength = 10)
      )
    })

    # Visual summary of shift test
    output$shiftPlot <- renderPlot({
      req(tables$erccShift, tables$erccStat_DE)
      dat <- tables$erccStat_DE
      if (is.null(dat) || !is.data.frame(dat)) {
        return(NULL)
      }

      theme_cols <- getOption("shinyRNAseq.theme.colors", list())
      fill_primary <- if (!is.null(theme_cols$primary)) theme_cols$primary else "#2FA4E7"
      fill_info <- if (!is.null(theme_cols$info)) theme_cols$info else "#5bc0de"
      point_col <- if (!is.null(theme_cols$danger)) theme_cols$danger else "#C71C22"
      grp <- unique(as.character(dat$GROUP))
      fill_vals <- rep(c(fill_primary, fill_info), length.out = length(grp))
      names(fill_vals) <- grp

      ggplot(dat, aes(x = GROUP, y = pct_ERCC, fill = GROUP)) +
        geom_boxplot(width = 0.5, outlier.alpha = 0.1, color = "black") +
        geom_jitter(color = point_col, width = 0.2, size = 2, alpha = 0.65) +
        scale_fill_manual(values = fill_vals) +
        theme_classic(base_size = 14) +
        labs(
          y = "Spike-in ratio (%)", x = "Group",
          title = paste0(
            "SHIFT = ", as.character(tables$erccShift$SHIFT),
            "  |  AUC = ", round(tables$erccShift$AUC, 3),
            "  |  Shift FC = ", round(tables$erccShift$Shift_FC, 3),
            "  |  P = ", format.pval(tables$erccShift$Shift_PVAL, digits = 3)
          )
        ) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none"
        )
    })

    # ERCC stat table for DE comparison
    output$erccStatDEtable <- DT::renderDT({
      req(tables$erccStat_DE)
      dat <- tables$erccStat_DE
      dat$pct_ERCC <- round(dat$pct_ERCC, 4)
      dat$nf_libSize <- round(dat$nf_libSize, 4)
      dat$nf_ERCC <- round(dat$nf_ERCC, 4)
      if ("ERCC_COR" %in% colnames(dat)) dat$ERCC_COR <- round(dat$ERCC_COR, 4)
      DT::datatable(dat,
        rownames = FALSE, class = "display compact",
        options = list(dom = "t", pageLength = 50)
      )
    })
  })
}
